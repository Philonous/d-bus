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


findProperty (Object o) ifaceName prop
    = case Map.lookup ifaceName o of
        Nothing -> Left noSuchInterface
        Just iface -> case find (\(SomeProperty p) -> propertyName p == prop)
                                $ interfaceProperties iface of
            Nothing -> Left noSuchProperty
            Just p -> Right p

handleProperty :: Object
               -> ObjectPath
               -> MemberName
               -> [SomeDBusValue]
               -> Either MsgError (MethodHandlerT IO SomeDBusArguments)
handleProperty o _ "Get" [mbIface, mbProp]
    | Just ifaceName <- fromRep =<< dbusValue mbIface
    , Just propName <- fromRep =<< dbusValue mbProp
    = findProperty o ifaceName propName
      >>= (\(SomeProperty prop) ->
            case propertyGet prop of
                Nothing -> Left propertyNotReadable
                Just rd -> Right $ SDBA . singletonArg <$> rd)
handleProperty o path "Set" [mbIface , mbProp, mbVal]
    | Just ifaceName <- fromRep =<< dbusValue mbIface
    , Just propName <- fromRep =<< dbusValue mbProp
    = findProperty o ifaceName propName
      >>= (\(SomeProperty prop@Property{propertySet = set}) -> do
          wt <- maybe (Left propertyNotWriteable) Right set
          v <- maybe (Left argTypeMismatch) Right (dbusValue mbVal)
          Right $ do
              invalidated <- wt v
              when invalidated $ propertyChanged prop v
              return (SDBA ArgsNil))
handleProperty _ _ _ _ = Left argTypeMismatch

callAtPath :: Objects
           -> ObjectPath
           -> Text.Text
           -> Text.Text
           -> [SomeDBusValue]
           -> Either MsgError (MethodHandlerT IO SomeDBusArguments)
callAtPath (Objects root) path interface member args = case Map.lookup path root of
    Nothing -> Left (MsgError "org.freedesktop.DBus.Error.Failed"
                                     (Just . Text.pack $ "No such object "
                                                          ++ show path)
                                     [])
    Just obj@(Object o) -> case interface of
        "org.freedesktop.DBus.Properties" -> handleProperty obj path member args
        _ -> case Map.lookup interface o of
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
