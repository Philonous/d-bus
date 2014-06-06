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

module DBus.Object where

import           Control.Applicative ((<$>))
import           Control.Monad
import           Data.List (intercalate, find)
import           Data.Maybe
import           Data.Singletons
import           Data.Singletons.Prelude.List
import           Data.Singletons.TH
import qualified Data.Text as Text

import           DBus.Types

-- | IsMethod is a Helper class to create MethodWrappers without having to
-- explicitly unwrap all the function arguments.
class IsMethod f where
    type ArgTypes f :: [DBusType]
    type ResultType f :: [DBusType]
    toMethod :: f -> MethodWrapper (ArgTypes f) (ResultType f)

instance SingI t => IsMethod (IO (DBusArguments t)) where
    type ArgTypes (IO (DBusArguments ts)) = '[]
    type ResultType (IO (DBusArguments ts)) = ts
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

flattenRep :: ( Representable a
              , SingI (RepType a)
              , SingI (FlattenRepType (RepType a))
              ) =>
              a
           -> DBusArguments (FlattenRepType (RepType a))
flattenRep (x :: t) =
    let rts = sing :: Sing (RepType t)
        frts = sing :: Sing (FlattenRepType (RepType t))
    in case (rts, frts) of
        (STypeUnit, SNil) -> ArgsNil
        (STypeStruct ts, ts') -> case toRep x of DBVStruct str -> structToArgs str
        (t, SCons t' SNil) -> case t %~ t' of
            Proved Refl -> ArgsCons (toRep x) ArgsNil
            Disproved _ -> error "flattenRep: this shouldn't happen"

instance (Representable t, SingI (RepType t), SingI (FlattenRepType (RepType t)))
         => RepMethod (IO t) where
    type RepMethodArgs (IO t) = '[]
    type RepMethodValue (IO t) = FlattenRepType (RepType t)
    repMethod f = MReturn $ flattenRep `liftM` f

instance (RepMethod b, Representable a, SingI (RepType a) )
         => RepMethod (a -> b) where
    type RepMethodArgs (a -> b) = (RepType a ': RepMethodArgs b)
    type RepMethodValue (a -> b) = RepMethodValue b
    repMethod f = MAsk  $ \x -> case fromRep x of
        Nothing -> error "marshalling error" -- TODO
        Just x -> repMethod $ f x

-- type family ArgumentRep a where
--     ArgumentRep () = '[]
--     ArgumentRep (DBusStruct ts) = ts
--     ArgumentRep a = '[RepType a]

-- class IsDBusArguments a where
--     toArguments :: a -> DBusArguments (ArgumentRep a)
--     fromArguments :: DBusArguments (ArgumentRep a) -> a


runMethodW :: SingI at =>
               MethodWrapper at rt
            -> [SomeDBusValue]
            -> Maybe (IO (DBusArguments rt))
runMethodW m args = runMethodW' sing args m

runMethodW' :: Sing at
             -> [SomeDBusValue]
             -> MethodWrapper at rt
             -> Maybe (IO (DBusArguments rt))
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


runMethod :: Method -> [SomeDBusValue] -> Maybe (IO SomeDBusArguments)
runMethod (Method m _ _) args = liftM SDBA <$> runMethodW m args

methodSignature :: Method -> ([DBusType], [DBusType])
methodSignature (Method m _ _) = methodWSignature m

methodName :: Method -> Text.Text
methodName (Method _ n _) = n

argDescriptions :: MethodDescription ts rs -> ([Text.Text], [Text.Text])
argDescriptions (t :-> ts) = let (ts', r) = argDescriptions ts in (t : ts', r)
argDescriptions (Result rs) = ([], dbusArgsToList rs)
  where
    dbusArgsToList :: ResultDescription t -> [Text.Text]
    dbusArgsToList ResultNil = []
    dbusArgsToList (t :> ts) = t : dbusArgsToList ts

instance Show Method where
    show m@(Method _ n desc) =
        let (args, res) = argDescriptions desc
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

callAtPath :: Object
           -> ObjectPath
           -> Text.Text
           -> Text.Text
           -> [SomeDBusValue]
           -> Either MsgError (IO SomeDBusArguments)
callAtPath root path interface member args = case findObject path root of
    Nothing -> Left (MsgError "org.freedesktop.DBus.Error.Failed"
                                     (Just . Text.pack $ "No such object "
                                                          ++ show path)
                                     [])
    Just o -> case find ((== interface) . interfaceName) $ objectInterfaces o of
        Nothing -> Left (MsgError "org.freedesktop.DBus.Error.Failed"
                                     (Just "No such interface")
                                     [])
        Just i -> case find ((== member) . methodName) $ interfaceMethods i of
            Nothing -> Left (MsgError "org.freedesktop.DBus.Error.Failed"
                                     (Just "No such interface")
                                     [])
            Just m -> case runMethod m args of
                Nothing -> Left (MsgError "org.freedesktop.DBus.Error.InvalidArgs"
                                          (Just "Argument type missmatch")
                                          [])
                Just ret -> Right ret
