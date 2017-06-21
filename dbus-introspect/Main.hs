{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Main where

import           Data.Char
import           Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           System.Environment
import           System.Exit
import           System.IO

import           DBus

main :: IO ()
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
showError err@(MethodErrorMessage (message : _)) =
  case message of
   DBV (DBVString msg) -> Text.unpack msg
   _ -> show err
showError err = show err
