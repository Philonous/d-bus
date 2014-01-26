{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternGuards #-}

module DBus.MainLoop where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import qualified Control.Exception as Ex
import           Control.Monad
import           Control.Monad.Fix (mfix)
import           Control.Monad.Trans
import           Data.Binary.Get as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Builder as BS
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Text as CT
import           Data.IORef
import qualified Data.Map as Map
import           Data.Singletons
import qualified Data.Text as Text
import           Data.Typeable (Typeable)
import           Data.Word
import           Network.Socket
import           System.Environment
import           System.IO
import           System.Log.Logger
import           System.Mem.Weak

import           Data.Attoparsec as AP
import           Data.List (intercalate)
import           Data.Monoid
import           Numeric

import           DBus.Auth
import           DBus.Error
import           DBus.Message
import           DBus.MessageBus
import           DBus.Object
import           DBus.Transport
import           DBus.Types
import           DBus.Wire

handleMessage handleCall handleSignals answerSlots (header, body) =
    case messageType header of
        MethodCall -> handleCall header body
        MethodReturn -> handleReturn True
        Error -> handleError
        Signal -> handleSignals header body
        _ -> return ()
  where
    handleReturn nonError = case hFReplySerial $ fields header of
        Nothing -> return ()
        Just s -> atomically $ do
            slots <- readTVar answerSlots
            let ret = case body of
                    [] -> DBV DBVUnit
                    (x:_) -> x
            case Map.lookup s slots of
                Nothing -> return ()
                Just putSlot -> do
                    writeTVar answerSlots (Map.delete s slots)
                    putSlot $ if nonError
                              then Right ret
                              else Left body
    handleError = case hFReplySerial $ fields header of
        Nothing -> return () -- TODO: handle non-response errors
        Just s -> handleReturn False

-- | Create a message handler that dispatches matches to the methods in a root
-- object
objectRoot :: Object -> Handler
objectRoot o conn header args | fs <- fields header
                              , Just path <- hFPath fs
                              , Just iface <- hFInterface fs
                              , Just member <- hFMember fs
                              , ser <- serial header
                              , Just sender <- hFSender fs
                              = Ex.handle (\e -> hPutStrLn stderr (show ( e:: Ex.SomeException))) $ do
    let errToErrMessage s e = errorMessage s (Just ser) sender (errorName e)
                                             (errorText e) (errorBody e)
        mkReturnMethod s arg = methodReturn s ser sender (filter notUnit [arg])
    serial <- atomically $ dBusCreateSerial conn
    ret <- case callAtPath o path iface member args of
        Left e -> return $ Left e
        Right f -> do
            ret <- withAsync (Ex.catch (Right <$> f) (return . Left)) waitCatch
            case ret of
                Left e -> return $ Left
                              (MsgError "org.freedesktop.DBus.Error.Failed"
                                        (Just $ "Method threw exception "
                                         `Text.append` Text.pack (show e)) [])
                Right r -> return $  r
    let bs = either (errToErrMessage serial) (mkReturnMethod serial) ret
    sendBS conn bs
  where notUnit (DBV DBVUnit) = False
        notUnit _ = True
objectRoot _ _ _ _ = return ()


-- | Check whether connection is alive
checkAlive :: DBusConnection -> IO Bool
checkAlive conn = atomically $ readTVar (connectionAliveRef conn)

-- | Wait until connection is closed
waitFor :: DBusConnection -> IO ()
waitFor conn = atomically $ do
    alive <- readTVar (connectionAliveRef conn)
    when alive retry

data ConnectionType = Session
                    | System
                    | Address String


-- | Create a new connection to a message bus
connectBus :: ConnectionType
           -> (DBusConnection -> MessageHeader -> [SomeDBusValue] -> IO ())
           -> (DBusConnection -> MessageHeader -> [SomeDBusValue] -> IO ())
           -> IO DBusConnection
connectBus transport handleCalls handleSignals = do
    addressString <- case transport of
        Session -> getEnv "DBUS_SESSION_BUS_ADDRESS"
        System -> do
            fromEnv <- Ex.try $ getEnv "DBUS_SYSTEM_BUS_ADDRESS"
            case fromEnv of
                Left (e :: Ex.SomeException) ->
                    return "unix:path=/var/run/dbus/system_bus_socket"
                Right addr -> return addr
        Address addr -> return addr
    debugM "DBus" $ "connecting to " ++ addressString
    mbS <- connectString addressString
    s <- case mbS of
        Nothing -> C.monadThrow (CouldNotConnect
                                   "All addresses failed to connect")
        Just s -> return s
    send s "\0"
    h <- socketToHandle s ReadWriteMode
    debugM "DBus" $ "Running SASL"
    runSasl (BS.hPutBuilder h) (BS.hGetLine h) external
    serialCounter <- newTVarIO 1
    let getSerial = do
            s <- readTVar serialCounter
            writeTVar serialCounter (s+1)
            return s
    lock <- newTMVarIO $ BS.hPutBuilder h
    answerSlots <- newTVarIO (Map.empty :: AnswerSlots)
    aliveRef <- newTVarIO True
    weakAliveRef <- mkWeakPtr aliveRef Nothing
    let kill = do
        mbRef <- deRefWeak weakAliveRef
        case mbRef of
             Nothing -> return ()
             Just ref -> atomically $ writeTVar ref False
        hClose h
        slots <- atomically $ do sls <- readTVar answerSlots
                                 writeTVar answerSlots Map.empty
                                 return sls
        atomically $ forM_ (Map.elems slots) $ \s -> s . Left $
                                        [DBV $ DBVString "Connection Closed"]
    mfix $ \conn' -> do
        debugM "DBus" $ "Forking"
        handlerThread <- forkIO $ Ex.catch (do
            CB.sourceHandle h
                C.$= parseMessages
                C.$$ (C.awaitForever $ liftIO .
                      handleMessage (handleCalls conn')
                                    (handleSignals conn')
                                     answerSlots))
            (\e -> print (e :: Ex.SomeException) >> kill >> Ex.throwIO e)
        addFinalizer aliveRef $ killThread handlerThread
        let conn = DBusConnection { dBusCreateSerial = getSerial
                                  , dBusAnswerSlots = answerSlots
                                  , dBusWriteLock = lock
                                  , dBusConnectionName = ""
                                  , connectionAliveRef = aliveRef
                                  }
        debugM "DBus" $ "hello"
        connName <- hello conn
        debugM "DBus" $ "Done"
        return conn{dBusConnectionName = connName}

type Handler = DBusConnection -> MessageHeader -> [SomeDBusValue] -> IO ()
