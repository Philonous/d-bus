{-# LANGUAGE OverloadedStrings #-}
module DBus.Signal where

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Catch (MonadThrow)
import           Control.Monad.Trans
import           Control.Monad.Writer
import qualified Data.List as List
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextL
import qualified Data.Text.Lazy.Builder as TB

import           DBus.Types
import           DBus.Message
import           DBus.MessageBus

data MatchRule = MatchRule { mrType          :: Maybe MessageType
                           , mrSender        :: Maybe Text.Text
                           , mrInterface     :: Maybe Text.Text
                           , mrMember        :: Maybe Text.Text
                           , mrPath          :: Maybe (Bool, ObjectPath)
                           , mrDestination   :: Maybe Text.Text
                           , mrArgs          :: [(Int,Text.Text)]
                           , mrArgPaths      :: [(Int,Text.Text)]
                           , mrArg0namespace :: Maybe Text.Text
                           , mrEavesdrop     :: Maybe Bool
                           }

matchAll :: MatchRule
matchAll = MatchRule Nothing Nothing Nothing Nothing Nothing Nothing
                     [] [] Nothing Nothing

renderRule :: MatchRule -> Text.Text
renderRule mr = Text.concat . TextL.toChunks . TB.toLazyText .
                   mconcat . List.intersperse (TB.singleton ',') $
            (catMaybes
                [ toRule "type"          fromMessageType  <$> mrType mr
                , toRule "sender"        id               <$> mrSender mr
                , toRule "interface"     id               <$> mrInterface mr
                , toRule "member"        id               <$> mrMember mr
                , (\(namespace, path) ->
                    toRule ("path" <> if namespace then "_namespace" else mempty)
                           objectPathToText path) <$> mrPath mr
                , toRule "destination"   id               <$> mrDestination mr
                , toRule "arg0namespace" id               <$> mrArg0namespace mr
                , toRule "eavesdrop"     boolToText       <$> mrEavesdrop mr
                ])
                ++ ((\(i, v) -> toRule ("arg" <> num i) id v) <$> mrArgs mr)
                ++ ((\(i, v) -> toRule ("arg" <> num i <> "path") id v)
                       <$> mrArgPaths mr)
  where
    toRule name toValue v = name
                           <> "='"
                           <> TB.fromText (toValue v)
                           <> TB.singleton '\''
    boolToText True  = "true"
    boolToText False = "false"
    fromMessageType MessageTypeMethodCall = "method_call"
    fromMessageType MessageTypeMethodReturn = "method_return"
    fromMessageType MessageTypeSignal = "signal"
    fromMessageType MessageTypeError = "error"
    ft = TB.fromText
    num i = TB.fromText . Text.pack $ show i

-- | Match a Signal against a rule. The argN, argNPath and arg0namespace
-- parameter are ignored at the moment
matchSignal :: MessageHeader -> MatchRule -> Bool
matchSignal header rule =
    let fs = fields header
    in and $ catMaybes
       [ Just $ messageType header == MessageTypeSignal
       , (\x -> hFMember fs == Just x ) <$> mrMember rule
       , (\x -> hFInterface fs == Just x ) <$> mrInterface rule
       , (\(ns, x) -> case hFPath fs of
               Nothing -> False
               Just p -> if ns then isPathPrefix x p
                               else x == p) <$> mrPath rule
       , (\x -> hFDestination fs == Just x) <$> mrDestination rule
       ]

addMatch :: (MonadIO m, MonadThrow m ) =>
            MatchRule
         -> DBusConnection
         -> m ()
addMatch rule = messageBusMethod "AddMatch" (renderRule rule)

removeMatch :: (MonadIO m, MonadThrow m ) =>
            MatchRule
         -> DBusConnection
         -> m ()
removeMatch rule = messageBusMethod "RemoveMatch" (renderRule rule)

----------------------
-- Emitting signals --
----------------------

signal :: Monad m => Signal -> MethodHandlerT m ()
signal sig = MHT $ tell [sig]

emitSignal :: Signal -> DBusConnection -> IO ()
emitSignal sig con = do
    sid <- atomically $ dBusCreateSerial con
    sendBS con $ mkSignal sid [] sig

execSignalT :: MethodHandlerT IO a -> DBusConnection -> IO (Either MsgError a)
execSignalT m con = do
    (x, sigs) <- runMethodHandlerT m
    forM_ sigs $ flip emitSignal con
    return x
