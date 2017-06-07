{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module DBus.Error where

import           Control.Exception as Ex
import           Data.Text         (Text)
import           Data.Typeable     (Typeable)

import           DBus.Types

data DBusError = CouldNotConnect String
               | DBusParseError String
               | MarshalError String
                 deriving (Show, Eq, Typeable)

instance Ex.Exception DBusError

errorFailed :: Text -> MsgError
errorFailed msg = (MsgError "org.freedesktop.DBus.Error.Failed"
                   (Just msg)
                   [])

noSuchInterface :: MsgError
noSuchInterface = errorFailed "No such interface"

noSuchProperty :: MsgError
noSuchProperty = errorFailed "No such property"

propertyNotReadable :: MsgError
propertyNotReadable = errorFailed "Property is not readable"

propertyNotWriteable :: MsgError
propertyNotWriteable = errorFailed "Property is not writeable"

argTypeMismatch :: MsgError
argTypeMismatch = errorFailed "Argument type mismatch"
