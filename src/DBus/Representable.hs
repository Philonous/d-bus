{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module DBus.Representable where

import           DBus.Types
import           DBus.TH

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad
import           Data.Int
import qualified Data.Map as Map
import qualified Data.Text as Text
import           Data.Word

-- class DBusRepresentable; see DBus.Types
forM [2..20] makeRepresentableTuple
