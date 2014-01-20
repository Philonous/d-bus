{-# LANGUAGE DeriveDataTypeable #-}
module DBus.Error where

import Control.Exception as Ex
import Data.Typeable (Typeable)

data DBusError = CouldNotConnect String
               | DBusParseError String
                 deriving (Show, Eq, Typeable)

instance Ex.Exception DBusError
