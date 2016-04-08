{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Main where

import           DBus
import           Data.Int
import           Data.Text (Text)
import qualified Data.Text as Text

main = do
  con <- connectClient Session
  result <- callMethod "net.sourceforge.liferea"
                       "/org/gnome/feed/Reader"
                       "org.gnome.feed.Reader"
                       "GetUnreadItems"
                       ()
                       []
                       con
                       :: IO (Either MethodError Int32)

  case result of
    Left e -> error $ "something went wrong " ++ show e
    Right unread -> putStrLn $ "We have " ++ show unread ++ " unread items"


unreadItems :: MethodDescription '[] '[ 'DBusSimpleType 'TypeInt32]
unreadItems =
  MD { methodObjectPath = "/org/gnome/feed/Reader"
     , methodInterface = "org.gnome.feed.Reader"
     , methodMember = "GetUnreadItems"
     , methodArgs = Done
     , methodResult = "unread items" :> Done
     }
