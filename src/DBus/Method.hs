{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module DBus.Method where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans
import qualified Data.List as List
import           Data.Singletons
import           Data.Singletons.Prelude.List
import           Data.Text (Text)
import qualified Data.Text as Text

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


instance (Representable t) => RepMethod (IO t) where
    type RepMethodArgs (IO t) = '[]
    type RepMethodValue (IO t) = FlattenRepType (RepType t)
    repMethod (f :: IO t)
        = let sng = sFlattenRepType (sing :: Sing (RepType t))
          in withSingI sng $ MReturn $ flattenRep . toRep <$> lift f

instance (Representable t) => RepMethod (SignalT IO t) where
    type RepMethodArgs (SignalT IO t) = '[]
    type RepMethodValue (SignalT IO t) = FlattenRepType (RepType t)
    repMethod (f :: SignalT IO t)
        = let sng = sFlattenRepType (sing :: Sing (RepType t))
          in withSingI sng $ MReturn $ flattenRep . toRep <$> f


instance (RepMethod b, Representable a)
         => RepMethod (a -> b) where
    type RepMethodArgs (a -> b) = (RepType a ': RepMethodArgs b)
    type RepMethodValue (a -> b) = RepMethodValue b
    repMethod f = MAsk  $ \x -> case fromRep x of
        Nothing -> error "marshalling error" -- TODO
        Just x -> repMethod $ f x

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
        in Text.unpack n ++ " :: " ++  List.intercalate " -> " components
