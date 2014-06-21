{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module DBus.Error where

import Control.Exception as Ex
import Data.Typeable (Typeable)

import DBus.Types

data DBusError = CouldNotConnect String
               | DBusParseError String
               | MarshalError String
                 deriving (Show, Eq, Typeable)

instance Ex.Exception DBusError

errorFailed msg = (MsgError "org.freedesktop.DBus.Error.Failed"
                   (Just msg)
                   [])

-- noSuchInterface, noSuchProperty, propertyNotReadable, propertyNotReadable
noSuchInterface = errorFailed "No such interface"
noSuchProperty = errorFailed "No such property"
propertyNotReadable = errorFailed "Property is not readable"
propertyNotWriteable = errorFailed "Property is not writeable"
argTypeMismatch = errorFailed "Argument type missmatch"
