{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE OverloadedStrings #-}

module DBus where

import           Control.Applicative ((<$>))
import           Control.Concurrent.STM
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Error
import           Data.List (find)
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import           Data.Word

import           Data.Singletons hiding (Error)
import           Data.Singletons.List


import           DBus.Introspect
import           DBus.Marshal
import           DBus.Object
import           DBus.Representable
import           DBus.Types
import           DBus.Mainloop

-- import qualified DBus.Connection as DBus


-- echo :: Text.Text ->  IO ()
-- echo txt = Text.putStrLn txt

-- checkLen :: Text.Text -> Word8 -> IO Bool
-- checkLen txt len = return $ Text.length txt > fromIntegral len

-- echoMethod = Method (repMethod echo) "echo" ("text" :-> Result "()")
-- checkLenMethod = Method (repMethod checkLen)
--                  "checklen"
--                  ("my little text" :-> "length" :-> Result "foo")

-- myInterface = Interface { interfaceName = "org.pontarius.dbus.testinterface"
--                         , interfaceMethods = [echoMethod, checkLenMethod]
--                         , interfaceAnnotations = []
--                         }

-- myObject = Object { objectObjectPath = objectPath "myObject"
--                   , objectInterfaces = [myInterface, introspectable myObject]
--                   , objectSubObjects = []
--                   }

-- root = Object { objectObjectPath = ObjectPath []
--               , objectInterfaces = [introspectable root]
--               , objectSubObjects = [myObject]
--               }


-- main = do
--     connect DBus.Session ["org.pontarius.test"] root
