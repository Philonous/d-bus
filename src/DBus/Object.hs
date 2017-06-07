{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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
import           Control.Monad
import           Control.Monad (liftM)
import           Data.List (find)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as Text

import           DBus.Types
import           DBus.Error
import           DBus.Property
import           DBus.Method


findProperty :: Object -> Text -> Text -> Either MsgError SomeProperty
findProperty (Object o) ifaceName prop
    = case Map.lookup ifaceName o of
        Nothing -> Left noSuchInterface
        Just iface -> case find (\(SomeProperty p) -> propertyName p == prop)
                                $ interfaceProperties iface of
            Nothing -> Left noSuchProperty
            Just p -> Right p

getAllProperties :: Interface -> MethodHandlerT IO SomeDBusArguments
getAllProperties iface = liftM (SDBA . singletonArg . toRep . mconcat)
                         . forM (interfaceProperties iface)
                         $ \(SomeProperty p) ->
    case propertyGet p of
        Nothing -> return (Map.empty :: Map Text (DBusValue 'TypeVariant))
        Just g -> flip catchMethodError (\_ -> return Map.empty) $ do
            res <-  g
            return $ Map.singleton (propertyName p) (DBVVariant res)

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
                Just rd -> Right $ SDBA . singletonArg . DBVVariant <$> rd)
handleProperty o _ "Set" [mbIface , mbProp, mbVal]
    | Just ifaceName <- fromRep =<< dbusValue mbIface
    , Just propName <- fromRep =<< dbusValue mbProp
    = findProperty o ifaceName propName
      >>= (\(SomeProperty prop@Property{propertySet = set}) -> do
          wt <- maybe (Left propertyNotWriteable) Right set
          v <- maybe (Left argTypeMismatch) Right
                   (fromVariant =<< dbusValue mbVal)
          Right $ do
              invalidated <- wt v
              when invalidated $ propertyChanged prop v
              return (SDBA ArgsNil))
handleProperty (Object o) _ "GetAll" [mbIface]
    | Just ifaceName <- fromRep =<< dbusValue mbIface
    = case Map.lookup ifaceName o of
        Just iface -> Right $ getAllProperties iface
        Nothing -> if ifaceName `elem` [""
                                        -- d-feet silliness:
                                       , "org.freedesktop.DBus.Properties"
                                       ]
                   then Right $ getAllProperties (mconcat $ Map.elems o)
                   else Left noSuchInterface
handleProperty _ _ _ _ = Left argTypeMismatch

callAtPath :: Objects
           -> ObjectPath
           -> Text.Text
           -> Text.Text
           -> [SomeDBusValue]
           -> Either MsgError (MethodHandlerT IO SomeDBusArguments)
callAtPath (Objects root') path interface member args = case Map.lookup path root' of
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
