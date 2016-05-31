{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Main where

import           Data.Char
import           Data.Maybe (fromMaybe, listToMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           System.Environment
import           System.Exit
import           System.IO

import           DBus

main = do
  args <- getArgs
  (bus, entity, root) <-
    case args of
     (busS : entityS : mbRoot) ->
       let bus :: ConnectionType
           bus = case toLower <$> busS of
                  "session" -> Session
                  "system" -> System
                  _ -> Address busS
       in return ( bus
                 , Text.pack entityS
                 , Text.pack $ fromMaybe "" (listToMaybe mbRoot)
                 )
     _ -> do
       hPutStrLn stderr "usage: dbus-introspect <bus> <entity> [<root>]"
       hPutStrLn stderr "    where <bus> is one of \"session\", \"system\" or an address"
       exitFailure
  con <- connectClient bus
  res <- callMethod entity (objectPath root) "org.freedesktop.DBus.Introspectable" "Introspect"
                    DBVUnit [] con
  case res of
   Left e -> hPutStrLn stderr $ "Error getting introspection data: " ++ showError e
   Right r -> Text.putStrLn r

showError :: MethodError -> String
showError error@(MethodErrorMessage (message : _)) =
  case message of
   DBV (DBVString message) -> Text.unpack message
   _ -> show error
showError error = show error
