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
import           Data.Singletons
import           Data.Singletons.Decide
import           Data.Singletons.Prelude.List
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextL
import qualified Data.Text.Lazy.Builder as TB

import           DBus.Types
import           DBus.Message
import           DBus.MessageBus
import           DBus.Representable


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
matchSignal :: Signal a -> MatchRule -> Bool
matchSignal sig rule = and $ catMaybes
       [ (\x -> signalMember sig ==  x ) <$> mrMember rule
       , (\x -> signalInterface sig == x ) <$> mrInterface rule
       , (\(ns, x) -> let p = (signalPath sig)
                      in if ns then isPathPrefix x p
                               else x == p) <$> mrPath rule
       ]

-- | Add a match rule
addMatch :: (MonadIO m, MonadThrow m ) =>
            MatchRule
         -> DBusConnection
         -> m ()
addMatch rule con = do
    let renderedRule = (renderRule rule)
    liftIO . logDebug $ "adding signal match rule: " ++ show renderedRule
    messageBusMethod "AddMatch" renderedRule con


-- | Remove a match rule
removeMatch :: (MonadIO m, MonadThrow m ) =>
            MatchRule
         -> DBusConnection
         -> m ()
removeMatch rule = messageBusMethod "RemoveMatch" (renderRule rule)


matchSignalToMatchRule :: MatchSignal -> MatchRule
matchSignalToMatchRule ms=
    matchAll { mrType      = Just MessageTypeSignal
             , mrInterface = matchInterface ms
             , mrMember    = matchMember ms
             , mrPath      = (False,) <$> matchPath ms
             , mrSender    = matchSender ms
             }
-- | Add a match rule for the given signal specification and call function on
-- all incoming matching signals
addSignalHandler :: MatchSignal
                 -> MatchRule -- Addition Match rules. mempty for none
                 -> (SomeSignal -> IO ())
                 -> DBusConnection
                 -> IO ()
addSignalHandler ms rules m dbc = do
    atomically $ modifyTVar (dbusSignalSlots dbc) ((fromSlot ms, m):)
    let rule = rules <> matchSignalToMatchRule ms
    addMatch rule dbc
  where
    fromSlot s = ( maybeToMatch $ matchInterface s
                 , maybeToMatch $ matchMember s
                 , maybeToMatch $ matchPath s
                 , maybeToMatch $ matchSender s)


castSignalBody :: SingI a => SomeSignal -> Maybe (DBusValue a)
castSignalBody (SomeSignal s) =
    case (signalBody s) of
     sr@(r :: DBusArguments ats) ->
         -- Use fix to access the return type (We only care about the type)
            fix $ \(_ :: Maybe (DBusValue ret)) ->
                  case sing :: Sing ret of
                      STypeStruct ts -> case (r, sing :: Sing ats) of
                          (ArgsNil, SNil) -> Nothing
                          (ArgsCons r' ArgsNil, SCons a SNil) ->
                              case a %~ (sing :: Sing ret) of
                                  Proved Refl -> Just r'
                                  Disproved _ -> Nothing
                          _ -> withSingI ts (DBVStruct <$> maybeArgsToStruct r)
                      STypeUnit -> case r of
                          ArgsNil -> Just DBVUnit
                          _ -> Nothing
                      _ -> case (sing :: Sing ats, r) of
                          (SCons at SNil, ArgsCons r' ArgsNil) ->
                              case at %~ (sing :: Sing ret) of
                                  Proved Refl -> Just r'
                                  Disproved _ -> Nothing


-- | Add a match rule (computed from the SignalDescription) and install a
-- handler that tries to convert the Signal's body and passes it to the callback
handleSignal :: Representable a =>
                SignalDescription (FlattenRepType (RepType a))
             -> Maybe Text
             -> MatchRule
             -> (a -> IO ())
             -> DBusConnection
             -> IO ()
handleSignal desc sender rules f con = do
    let mSignal = MatchSignal { matchInterface = Just $ signalDInterface desc
                              , matchMember = Just $ signalDMember desc
                              , matchPath = Just $ signalDPath desc
                              , matchSender = sender
                              }
        f' = \s -> case fromRep =<< castSignalBody s of
                       Nothing -> logWarning $ "Received signal "
                                             ++ ((\(SomeSignal ss) -> show ss) s)
                                             ++ " could not be converted to to"
                                             ++ " type "
                                             -- ++ (show . fromSing
                                             --       $ (sing :: Sing ts))
                       Just x -> f x
    addSignalHandler mSignal rules f' con

-- | Add a match rule for the given signal specification and put all incoming
-- signals into the TChan
signalChan :: MatchSignal
           -> DBusConnection
           -> IO (TChan SomeSignal)
signalChan match dbc = do
    signalChan <- newTChanIO
    addSignalHandler match mempty (atomically . writeTChan signalChan) dbc
    return signalChan

signalChan' :: Representable a =>
               SignalDescription (FlattenRepType (RepType a))
            -> Maybe Text
            -> MatchRule
            -> DBusConnection
            -> IO (TChan a)
signalChan' desc sender rules con = do
    signalChan <- newTChanIO
    handleSignal desc sender rules (atomically . writeTChan signalChan) con
    return signalChan

createSignal desc x = Signal{ signalPath = signalDPath desc
                            , signalInterface = signalDInterface desc
                            , signalMember = signalDMember desc
                            , signalBody = flattenRep $ toRep x
                            }

----------------------
-- Emitting signals --
----------------------

signal :: (Representable a, Monad m) =>
          SignalDescription (FlattenRepType (RepType a))
          -> a
          -> MethodHandlerT m ()
signal desc (x :: a) =
    let s = (sing :: Sing (RepType a))
        fs = sFlattenRepType s
        in withSingI fs $ signal' (SomeSignal $ createSignal desc x)

signal' :: Monad m => SomeSignal -> MethodHandlerT m ()
signal' sig = MHT $ tell [sig]

emitSignal' (SomeSignal s) con = do
    sid <- atomically $ dBusCreateSerial con
    logDebug $ "Emitting signal (ID = " ++ show sid ++ "): " ++ show s
    sendBS con $ mkSignal sid [] s

emitSignal :: Representable a =>
              SignalDescription (FlattenRepType (RepType a))
           -> a
           -> DBusConnection -> IO ()
emitSignal sigD (x :: a) con =
    let s = (sing :: Sing (RepType a))
        fs = sFlattenRepType s
    in withSingI fs $ emitSignal' (SomeSignal $ createSignal sigD x) con

execSignalT :: MethodHandlerT IO a -> DBusConnection -> IO (Either MsgError a)
execSignalT m con = do
    (x, sigs) <- runMethodHandlerT m
    forM_ sigs $ flip emitSignal' con
    return x
