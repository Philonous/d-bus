{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverlappingInstances #-}

module DBus.Object where

import           Control.Applicative ((<$>))
import           Control.Monad
import           Data.List (intercalate)
import           Data.Maybe
import qualified Data.Text as Text
import           Data.Singletons
import           Data.Singletons.TH
import           Data.Singletons.List

import           DBus.Connection
import           DBus.Marshal
-- import qualified DBus.Message as DBus
-- import           DBus.Types

class IsMethod f where
    type ArgTypes f
    type ResultType f
    toMethod :: f -> MethodWrapper (ArgTypes f) (ResultType f)

instance SingI t => IsMethod (IO (DBusValue t)) where
    type ArgTypes (IO (DBusValue t)) = '[]
    type ResultType (IO (DBusValue t)) = t
    toMethod = MReturn

instance (IsMethod f, SingI t) => IsMethod (DBusValue t -> f) where
    type ArgTypes (DBusValue t -> f) = (t ': ArgTypes f)
    type ResultType (DBusValue t -> f) = ResultType f
    toMethod f = MAsk $ \x -> toMethod (f x)

class RepMethod f where
    type RepMethodArgs f :: [DBusType]
    type RepMethodValue f :: DBusType
    repMethod :: f -> MethodWrapper (RepMethodArgs f) (RepMethodValue f)

instance (Representable t, SingI (RepType t)) => RepMethod (IO t) where
    type RepMethodArgs (IO t) = '[]
    type RepMethodValue (IO t) = RepType t
    repMethod f = MReturn $ toRep `liftM` f

instance (RepMethod b, Representable a, SingI (RepType a) )
         => RepMethod (a -> b) where
    type RepMethodArgs (a -> b) = (RepType a ': RepMethodArgs b)
    type RepMethodValue (a -> b) = RepMethodValue b
    repMethod f = MAsk  $ \x -> case fromRep x of
        Nothing -> error "marshalling error" -- TODO
        Just x -> repMethod $ f x

callMethodW :: SingI at =>
               MethodWrapper at rt
            -> [DBus.Arg]
            -> Maybe (IO (DBusValue rt))
callMethodW m args = callMethodW' sing args m

callMethodW' :: Sing at
             -> [DBus.Arg]
             -> MethodWrapper at rt
             -> Maybe (IO (DBusValue rt))
callMethodW' SNil         []         (MReturn f) = Just f
callMethodW' (SCons t ts) (arg:args) (MAsk f)    = (callMethodW' ts args . f )
                                                  =<< dbusValue (fromArg arg)
callMethodW' _            _           _          = Nothing

methodWSignature :: (SingI at, SingI rt) =>
                   MethodWrapper (at :: [DBusType]) (rt :: DBusType)
                -> ([DBusType], Maybe DBusType)
methodWSignature (_ :: MethodWrapper at rt) = ( fromSing (sing :: Sing at)
                                              , case fromSing (sing :: Sing rt) of
                                                     TypeUnit -> Nothing
                                                     t -> Just t)


callMethod :: Method -> [DBus.Arg] -> Maybe (IO SomeDBusValue)
callMethod (Method m _ _) args = liftM DBV <$> callMethodW m args

methodSignature :: Method -> ([DBusType], Maybe DBusType)
methodSignature (Method m _ _) = methodWSignature m

methodName :: Method -> Text.Text
methodName (Method _ n _) = n

argDescriptions :: MethodDescription t -> ([Text.Text], Text.Text)
argDescriptions (Result t) = ([], t)
argDescriptions (t :-> ts) = let (ts', r) = argDescriptions ts in (t : ts', r)

instance Show Method where
    show m@(Method _ n desc) =
        let (args, res) = argDescriptions desc
            (argst, rest) = methodSignature m
            components = zipWith (\name tp -> (Text.unpack name
                                               ++ ":"
                                               ++ ppType tp))
                                 (args ++ [res])
                                 (argst ++ [fromMaybe TypeUnit rest])
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
    Just suff -> if isRoot suff then Just o
                 else listToMaybe . catMaybes $
                        (findObject suff <$> objectSubObjects o)
