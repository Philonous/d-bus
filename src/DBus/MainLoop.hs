{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DBus.MainLoop where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import qualified Control.Exception as Ex
import           Control.Monad
import           Control.Monad.Catch (MonadThrow, throwM)
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
import           Foreign.C
import           Network.Socket
import           System.Environment
import           System.IO
import           System.Log.Logger
import           System.Mem.Weak

import           Data.Attoparsec.ByteString as AP
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
import           DBus.Signal
import           DBus.Property
import           DBus.Introspect

debug t = hPutStrLn stderr $ "d-bus debug: " ++ t

handleMessage :: (MessageHeader -> [SomeDBusValue] -> IO ())
              -> (MessageHeader -> [SomeDBusValue] -> IO a)
              -> TVar AnswerSlots
              -> TVar SignalSlots
              -> (MessageHeader, [SomeDBusValue])
              -> IO ()
handleMessage handleCall handleSignals answerSlots signalSlots (header, body) = do
    case messageType header of
        MessageTypeMethodCall -> do
            let hfs = fields header
            debug $ "Dispatching method call "
                ++ show (hFPath hfs)
                ++ "; " ++ (maybe "" Text.unpack $ hFInterface hfs)
                ++ "; " ++ (maybe "" Text.unpack $ hFMember hfs)
                ++ ": " ++ show body
            handleCall header body
        MessageTypeMethodReturn -> handleReturn True
        MessageTypeError -> handleError
        MessageTypeSignal -> handleSignal
        _ -> return ()
  where
    handleReturn nonError = case hFReplySerial $ fields header of
        Nothing -> return ()
        Just s -> atomically $ do
            slots <- readTVar answerSlots
            case Map.lookup s slots of
                Nothing -> return ()
                Just putSlot -> do
                    writeTVar answerSlots (Map.delete s slots)
                    putSlot $ if nonError
                              then Right body
                              else Left body
    handleError = case hFReplySerial $ fields header of
        Nothing -> return () -- TODO: handle non-response errors
        Just s -> handleReturn False
    handleSignal = do
      handleSignals header body
      sSlots <- atomically $ readTVar signalSlots
      let fs = fields header
      case () of
         _ | Just iface <- hFInterface fs
           , Just member <- hFMember fs
           , Just path <- hFPath fs
           , Just sender <- hFSender fs
             -> case Map.lookup ( Match iface
                                , Match member
                                , Match path
                                , Match sender)
                                sSlots of
                    Just handler ->
                        let sig = Signal { signalPath = path
                                         , signalInterface = iface
                                         , signalMember = member
                                         , signalBody = body
                                         }
                        in handler sig
                    _  -> debug $ "Unhandled signal"
                                   ++ show iface ++ "; "
                                   ++ show member ++ "; "
                                   ++ show path ++ "; "
                                   ++ show sender ++ ": "
                                   ++ show body ++ "\n"
           | otherwise -> debug $ "Signal is missing header fields:"
                                       ++ show header ++ "; " ++ show body

-- | Create a message handler that dispatches matches to the methods in a root
-- object
objectRoot :: Objects -> Handler
objectRoot o conn header args | fs <- fields header
                              , Just path <- hFPath fs
                              , Just iface <- hFInterface fs
                              , Just member <- hFMember fs
                              , ser <- serial header
                              , Just sender <- hFSender fs
                              = Ex.handle (\e -> hPutStrLn stderr (show ( e:: Ex.SomeException))) $ do
    let errToErrMessage s e = errorMessage s (Just ser) sender (errorName e)
                                             (errorText e) (errorBody e)
        mkReturnMethod s args = methodReturn s ser sender args
    (ret, sigs) <- case callAtPath o path iface member args of
        Left e -> return (Left e, [])
        Right f -> do
            ret <- withAsync (Ex.catch (runMethodHandlerT f)
                                       -- catches MsgError only:
                                       (\e -> return (Left e, []))
                             ) waitCatch
            case ret of
                Left e -> return $ (Left
                              (MsgError "org.freedesktop.DBus.Error.Failed"
                                        (Just $ "Method threw exception: "
                                         `Text.append` Text.pack (show e)) [])
                                   , [])
                Right r -> return r
    serial <- atomically $ dBusCreateSerial conn
    forM_ sigs $ flip emitSignal conn
    debug $ "method call returned " ++ show ret
    case ret of
        Left err -> sendBS conn $ errToErrMessage serial err
        Right r -> sendBS conn $ mkReturnMethod serial r
    debug "done"

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

-- | Which Bus to connect to
data ConnectionType = System -- ^ The well-known system bus. First
                              -- the environmental variable
                              -- DBUS_SYSTEM_BUS_ADDRESS is checked and if it
                              -- doesn't exist the address
                              -- /unix:path=\/var\/run\/dbus\/system_bus_socket/
                              -- is used
                    | Session -- ^ The well-known session bus. Refers to the
                              -- address stored in the environmental variable
                              -- DBUS_SESSION_BUS_ADDRESS
                    | Address String -- ^ The bus at the give addresss

type MethodCallHandler = ( DBusConnection
                           -> MessageHeader
                           -> [SomeDBusValue]
                           -> IO ())

type SignalHandler = ( DBusConnection
                       -> MessageHeader
                       -> [SomeDBusValue]
                       -> IO ())

-- | Create a new connection to a message bus
connectBus :: ConnectionType -- ^ Bus to connect to
           -> MethodCallHandler -- ^ Handler for incoming method calls
           -> SignalHandler  -- ^ Handler for incoming signals
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
        Nothing -> throwM (CouldNotConnect
                                   "All addresses failed to connect")
        Just s -> return s
    sendCredentials s
    h <- socketToHandle s ReadWriteMode
    debugM "DBus" $ "Running SASL"
    runSasl (\bs -> do
                  debugM "DBus.Sasl" $ "C: " ++ show (BS.toLazyByteString bs)
                  BS.hPutBuilder h bs)
            (do
                  bs <- BS.hGetLine h
                  debugM "DBus.Sasl" $ "S: " ++ show bs
                  return bs)
            external
    serialCounter <- newTVarIO 1
    let getSerial = do
            s <- readTVar serialCounter
            writeTVar serialCounter (s+1)
            return s
    lock <- newTMVarIO $ BS.hPutBuilder h
    answerSlots <- newTVarIO (Map.empty :: AnswerSlots)
    signalSlots <- newTVarIO (Map.empty :: SignalSlots)
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
        atomically $ writeTVar signalSlots Map.empty
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
                                    answerSlots
                                    signalSlots ))
            (\e -> print (e :: Ex.SomeException) >> kill >> Ex.throwIO e)
        addFinalizer aliveRef $ killThread handlerThread
        let conn = DBusConnection { dBusCreateSerial = getSerial
                                  , dBusAnswerSlots = answerSlots
                                  , dbusSignalSlots = signalSlots
                                  , dBusWriteLock = lock
                                  , dBusConnectionName = ""
                                  , connectionAliveRef = aliveRef
                                  }
        debugM "DBus" $ "hello"
        connName <- hello conn
        debugM "DBus" $ "Done"
        return conn{dBusConnectionName = connName}

makeServer :: ConnectionType -> Objects -> IO DBusConnection
makeServer transport objs = do
    connectBus transport (objectRoot (addIntrospectable objs))
                         (\_ _ _ -> return ())

type Handler = DBusConnection -> MessageHeader -> [SomeDBusValue] -> IO ()

sendCredentials :: Socket -> IO Int
#ifdef SEND_CREDENTIALS
foreign import ccall "send_credentials_and_zero"
    sendCredentialsAndZero :: CInt -> IO CInt

sendCredentials (MkSocket si _ _ _ _) = fromIntegral <$> sendCredentialsAndZero si
#else
sendCredentials s = send s "\0"
#endif
