{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module DBus.Signal where

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Catch (MonadThrow)
import           Control.Monad.Trans
import           Control.Monad.Writer
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as Map
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

-- Left-biased monoid
instance Monoid MatchRule where
    mempty = matchAll
    mappend lr rr =
        MatchRule
            { mrType          = mrType          lr `mplus` mrType          rr
            , mrSender        = mrSender        lr `mplus` mrSender        rr
            , mrInterface     = mrInterface     lr `mplus` mrInterface     rr
            , mrMember        = mrMember        lr `mplus` mrMember        rr
            , mrPath          = mrPath          lr `mplus` mrPath          rr
            , mrDestination   = mrDestination   lr `mplus` mrDestination   rr
            , mrArgs          = mrArgs          lr `mplus` mrArgs          rr
            , mrArgPaths      = mrArgPaths      lr `mplus` mrArgPaths      rr
            , mrArg0namespace = mrArg0namespace lr `mplus` mrArg0namespace rr
            , mrEavesdrop     = mrEavesdrop     lr `mplus` mrEavesdrop     rr
            }



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

-- | Add a match rule
addMatch :: (MonadIO m, MonadThrow m ) =>
            MatchRule
         -> DBusConnection
         -> m ()
addMatch rule = messageBusMethod "AddMatch" (renderRule rule)


-- | Remove a match rule
removeMatch :: (MonadIO m, MonadThrow m ) =>
            MatchRule
         -> DBusConnection
         -> m ()
removeMatch rule = messageBusMethod "RemoveMatch" (renderRule rule)


-- | Add a match rule for the given signal specification and call function on
-- all incoming matching signals
addSignalHandler :: MatchSignal
                 -> MatchRule -- Addition Match rules. mempty for none
                 -> (Signal -> IO ())
                 -> DBusConnection
                 -> IO ()
addSignalHandler slot rules m dbc = do
    atomically $ modifyTVar (dbusSignalSlots dbc) (Map.insert (fromSlot slot) m)
    let rule = rules <>
                 matchAll { mrType      = Just MessageTypeSignal
                          , mrInterface = matchInterface slot
                          , mrMember    = matchMember slot
                          , mrPath      = (False,) <$> matchPath slot
                          , mrSender    = matchSender slot
                          }
    addMatch rule dbc
  where
    fromSlot s = ( maybeToMatch $ matchInterface s
                 , maybeToMatch $ matchMember s
                 , maybeToMatch $ matchPath s
                 , maybeToMatch $ matchSender s)


-- | Add a match rule for the given signal specification and put all incoming
-- signals into the TChan
signalChan :: MatchSignal
           -> DBusConnection
           -> IO (TChan Signal)
signalChan match dbc = do
    signalChan <- newTChanIO
    addSignalHandler match mempty (atomically . writeTChan signalChan) dbc
    return signalChan

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
