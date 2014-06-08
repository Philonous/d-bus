{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PatternGuards #-}

module DBus.Object where

import           Control.Applicative ((<$>))
import           Control.Concurrent.STM
import qualified Control.Exception as Ex
import           Control.Monad
import           Control.Monad.Trans
import           Data.List (intercalate, find)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Singletons
import           Data.Singletons.Prelude.List
import           Data.Singletons.TH
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as Text
import           Unsafe.Coerce (unsafeCoerce)

import           DBus.Types
import           DBus.Representable

-- | IsMethod is a Helper class to create MethodWrappers without having to
-- explicitly unwrap all the function arguments.
class IsMethod f where
    type ArgTypes f :: [DBusType]
    type ResultType f :: [DBusType]
    toMethod :: f -> MethodWrapper (ArgTypes f) (ResultType f)

instance SingI t => IsMethod (IO (DBusArguments t)) where
    type ArgTypes (IO (DBusArguments ts)) = '[]
    type ResultType (IO (DBusArguments ts)) = ts
    toMethod = MReturn. lift

instance SingI t => IsMethod (SignalT IO (DBusArguments t)) where
    type ArgTypes (SignalT IO (DBusArguments ts)) = '[]
    type ResultType (SignalT IO (DBusArguments ts)) = ts
    toMethod = MReturn

instance (IsMethod f, SingI t) => IsMethod (DBusValue t -> f) where
    type ArgTypes (DBusValue t -> f) = (t ': ArgTypes f)
    type ResultType (DBusValue t -> f) = ResultType f
    toMethod f = MAsk $ \x -> toMethod (f x)

class RepMethod f where
    type RepMethodArgs f :: [DBusType]
    type RepMethodValue f :: [DBusType]
    repMethod :: f -> MethodWrapper (RepMethodArgs f) (RepMethodValue f)

type family FlattenRepType r where
    FlattenRepType TypeUnit = '[]
    FlattenRepType (TypeStruct ts) = ts
    FlattenRepType t = '[t]

-- TODO: Figure out how to convice GHC that the flattenRepType function and the
-- FlattenRepType type function coincide
flattenRepS :: Sing a -> Sing (FlattenRepType a)
flattenRepS s = case flattenRepS' s of
    SomeSing s' -> unsafeCoerce s'
  where
    flattenRepS' STypeUnit = SomeSing SNil
    flattenRepS' (STypeStruct ts) = SomeSing ts
    flattenRepS' t = SomeSing (SCons t SNil)

flattenRep :: ( Representable a ) =>
              a
           -> DBusArguments (FlattenRepType (RepType a))
flattenRep (x :: t) =
    let rts = sing :: Sing (RepType t)
        frts :: Sing (FlattenRepType (RepType t))
        frts = flattenRepS rts
    in case (rts, frts) of
        (STypeUnit, SNil) -> ArgsNil
        (STypeStruct ts, ts') -> case toRep x of DBVStruct str -> structToArgs str
        (t, SCons t' SNil) -> case t %~ t' of
            Proved Refl -> ArgsCons (toRep x) ArgsNil
            Disproved _ -> error "flattenRep: this shouldn't happen"

instance (Representable t) => RepMethod (IO t) where
    type RepMethodArgs (IO t) = '[]
    type RepMethodValue (IO t) = FlattenRepType (RepType t)
    repMethod (f :: IO t)
        = let sng = flattenRepS (sing :: Sing (RepType t))
          in withSingI sng $ MReturn $ flattenRep . toRep <$> lift f

instance (Representable t) => RepMethod (SignalT IO t) where
    type RepMethodArgs (SignalT IO t) = '[]
    type RepMethodValue (SignalT IO t) = FlattenRepType (RepType t)
    repMethod (f :: SignalT IO t)
        = let sng = flattenRepS (sing :: Sing (RepType t))
          in withSingI sng $ MReturn $ flattenRep . toRep <$> f


instance (RepMethod b, Representable a)
         => RepMethod (a -> b) where
    type RepMethodArgs (a -> b) = (RepType a ': RepMethodArgs b)
    type RepMethodValue (a -> b) = RepMethodValue b
    repMethod f = MAsk  $ \x -> case fromRep x of
        Nothing -> error "marshalling error" -- TODO
        Just x -> repMethod $ f x

-- | Create a property from a getter and a setter. It will emit a
-- PropertyChanged signal when the setter is called (not only then). To change
-- this behaviour modify the propertyEmitsChangedSignal field
mkProperty :: Representable a =>
              Text
           -> Maybe (IO a)
           -> Maybe (a -> IO Bool)
           -> Property
mkProperty name get set =
    let pw = PropertyWrapper { setProperty = doSet <$> set
                             , getProperty = lift . fmap toRep <$> get
                             }
        in Property { propertyName = name
                    , propertyAccessors = pw
                    , propertyEmitsChangedSignal = PECSTrue
                    }
  where
    doSet f v = lift $ f =<< fromRepHelper v
    fromRepHelper x = case fromRep x of
        Nothing -> Ex.throwIO argTypeMismatch
        Just r -> return r

-- | Make a property out of a TVar. The property is considered changed on every
-- get, no matter if the updated value is actually different from the old one
mkTVarProperty :: Representable a =>
                  Text
               -> TVar a
               -> Property
mkTVarProperty name tv = mkProperty
                          name
                           (Just (atomically $ readTVar tv))
                           (Just (\v -> atomically (writeTVar tv v)
                                        >> return True))

runMethodW :: SingI at =>
               MethodWrapper at rt
            -> [SomeDBusValue]
            -> Maybe (SignalT IO (DBusArguments rt))
runMethodW m args = runMethodW' sing args m

runMethodW' :: Sing at
             -> [SomeDBusValue]
             -> MethodWrapper at rt
             -> Maybe (SignalT IO (DBusArguments rt))
runMethodW' SNil         []         (MReturn f) = Just f
runMethodW' (SCons t ts) (arg:args) (MAsk f)    = (runMethodW' ts args . f )
                                                  =<< dbusValue arg
runMethodW' _            _           _          = Nothing

methodWSignature :: (SingI at, SingI rt) =>
                   MethodWrapper (at :: [DBusType]) (rt :: [DBusType])
                -> ([DBusType], [DBusType])
methodWSignature (_ :: MethodWrapper at rt) =
    ( fromSing (sing :: Sing at)
    , fromSing (sing :: Sing rt)
    )


runMethod :: Method -> [SomeDBusValue] -> Maybe (SignalT IO SomeDBusArguments)
runMethod (Method m _ _ _) args = liftM SDBA <$> runMethodW m args

methodSignature :: Method -> ([DBusType], [DBusType])
methodSignature (Method m _ _ _) = methodWSignature m

methodName :: Method -> Text.Text
methodName (Method _ n _ _) = n

argDescToList :: ArgumentDescription ts
                -> [Text.Text]
argDescToList (t :-> ts) = t :  argDescToList ts
argDescToList Result = []

resultDescToList :: ResultDescription t -> [Text.Text]
resultDescToList ResultDone = []
resultDescToList (t :> ts) = t : resultDescToList ts

argDescriptions args ress = (argDescToList args, resultDescToList ress)

instance Show Method where
    show m@(Method _ n argDs resDs) =
        let (args, res) = argDescriptions argDs resDs
            (argst, rest) = methodSignature m
            components = zipWith (\name tp -> (Text.unpack name
                                               ++ ":"
                                               ++ ppType tp))
                                 (args ++ res)
                                 (argst ++ rest)
        in Text.unpack n ++ " :: " ++  intercalate " -> " components

instance Show Interface where
    show i = "Interface " ++ show (interfaceName i) ++ " [{"
              ++ intercalate "}, {" (map show $ interfaceMethods i) ++ "}]"

instance Show Object where
    show o = "Object " ++ show (objectPathToText $ objectObjectPath o)  ++ " [("
             ++ intercalate "), (" (map show $ objectInterfaces o) ++ ")]"


findObject :: ObjectPath -> Object -> Maybe Object
findObject path o = case stripObjectPrefix (objectObjectPath o) path of
    Nothing -> Nothing
    Just suff -> if isEmpty suff then Just o
                 else listToMaybe . catMaybes $
                        (findObject suff <$> objectSubObjects o)

errorFailed msg = (MsgError "org.freedesktop.DBus.Error.Failed"
                   (Just msg)
                   [])

-- noSuchInterface, noSuchProperty, propertyNotReadable, propertyNotReadable
noSuchInterface = errorFailed "No such interface"
noSuchProperty = errorFailed "No such property"
propertyNotReadable = errorFailed "Property is not readable"
propertyNotWriteable = errorFailed "Property is not writeable"
argTypeMismatch = errorFailed "Argument type missmatch"

findProperty o ifaceName prop
    = case find ((== ifaceName) . interfaceName) $ objectInterfaces o of
        Nothing -> Left noSuchInterface
        Just iface -> case find ((== prop) . propertyName)
                              $ interfaceProperties iface of
            Nothing -> Left noSuchProperty
            Just p -> Right p

propertyInterface = "org.freedesktop.DBus.Properties"

handleProperty :: Object
               -> ObjectPath
               -> MemberName
               -> [SomeDBusValue]
               -> Either MsgError (SignalT IO SomeDBusArguments)
handleProperty o _ "Get" [mbIface, mbProp]
    | Just ifaceName <- fromRep =<< dbusValue mbIface
    , Just propName <- fromRep =<< dbusValue mbProp
    = findProperty o ifaceName propName
      >>= (\Property{ propertyAccessors = accs} ->
            case getProperty accs of
                Nothing -> Left propertyNotReadable
                Just rd -> Right $ SDBA . singletonArg <$> rd)
handleProperty o path "Set" [mbIface , mbProp, mbVal]
    | Just ifaceName <- fromRep =<< dbusValue mbIface
    , Just propName <- fromRep =<< dbusValue mbProp
    = findProperty o ifaceName propName
      >>= (\prop@Property{ propertyAccessors = accs} -> do
          rd <- maybe (Left propertyNotWriteable) Right (setProperty accs)
          v <- maybe (Left argTypeMismatch) Right (dbusValue mbVal)
          Right $ do
              invalidated <- rd v
              when invalidated $ case propertyEmitsChangedSignal prop of
                  PECSTrue ->
                      signal
                        Signal
                          { signalPath = path
                          , signalInterface = propertyInterface
                          , signalMember = "PropertiesChanged"
                          , signalBody =
                              [ DBV $
                                toRep ( ifaceName
                                      , Map.fromList [(propName , DBVVariant v)]
                                      , [] :: [Text]
                                      )
                              ]
                          }
                  PECSInvalidates ->
                      signal
                        Signal
                          { signalPath = path
                          , signalInterface = propertyInterface
                          , signalMember = "PropertiesChanged"
                          , signalBody =
                              [ DBV $ toRep
                                ( ifaceName
                                , Map.empty :: Map.Map Text
                                                       (DBusValue (TypeVariant))
                                , [propName]
                                )
                              ]
                          }
                  PECSFalse -> return ()
              return (SDBA ArgsNil))
handleProperty _ _ _ _ = Left argTypeMismatch

callAtPath :: Object
           -> ObjectPath
           -> Text.Text
           -> Text.Text
           -> [SomeDBusValue]
           -> Either MsgError (SignalT IO SomeDBusArguments)
callAtPath root path interface member args = case findObject path root of
    Nothing -> Left (MsgError "org.freedesktop.DBus.Error.Failed"
                                     (Just . Text.pack $ "No such object "
                                                          ++ show path)
                                     [])
    Just o -> case interface of
        "org.freedesktop.DBus.Properties" -> handleProperty o path member args
        _ -> case find ((== interface) . interfaceName) $ objectInterfaces o of
            Nothing -> Left noSuchInterface
            Just i -> case find ((== member) . methodName) $ interfaceMethods i of
                Nothing -> Left (MsgError "org.freedesktop.DBus.Error.Failed"
                                         (Just "No such member")
                                         [])
                Just m -> case runMethod m args of
                    Nothing ->
                        Left (MsgError "org.freedesktop.DBus.Error.InvalidArgs"
                              (Just "Argument type missmatch")
                              [])
                    Just ret -> Right ret
