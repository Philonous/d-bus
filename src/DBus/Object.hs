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
import           Control.Monad (liftM)
import           Control.Monad.Trans
import           Data.List (intercalate, find)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Singletons.TH
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as Text
import           Unsafe.Coerce (unsafeCoerce)

import           DBus.Types
import           DBus.Representable
import           DBus.Error
import           DBus.Property
import           DBus.Signal
import           DBus.Method

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

findProperty o ifaceName prop
    = case find ((== ifaceName) . interfaceName) $ objectInterfaces o of
        Nothing -> Left noSuchInterface
        Just iface -> case find ((== prop) . propertyName)
                              $ interfaceProperties iface of
            Nothing -> Left noSuchProperty
            Just p -> Right p

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
                Just rd -> Right $ SDBA . singletonArg <$> lift rd)
handleProperty o path "Set" [mbIface , mbProp, mbVal]
    | Just ifaceName <- fromRep =<< dbusValue mbIface
    , Just propName <- fromRep =<< dbusValue mbProp
    = findProperty o ifaceName propName
      >>= (\prop@Property{ propertyAccessors = accs} -> do
          rd <- maybe (Left propertyNotWriteable) Right (setProperty accs)
          v <- maybe (Left argTypeMismatch) Right (dbusValue mbVal)
          Right $ do
              invalidated <- rd v
              when invalidated $ propertyChanged prop
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
