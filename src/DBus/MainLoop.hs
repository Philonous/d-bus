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

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import qualified Control.Exception            as Ex
import           Control.Monad
import           Control.Monad.Catch          (throwM)
import           Control.Monad.Fix            (mfix)
import           Control.Monad.Trans
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy.Builder as BS
import qualified Data.Conduit                 as C
import qualified Data.Conduit.Binary          as CB
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Network.Socket               (Socket, socketToHandle)
import           Network.Socket.ByteString    (send)
import           System.Environment
import           System.IO
import           System.Log.Logger
import           System.Mem.Weak

import           DBus.Auth
import           DBus.Error
import           DBus.Message
import           DBus.MessageBus
import           DBus.Object
import           DBus.Transport
import           DBus.Types
import           DBus.Signal
import           DBus.Introspect

handleMessage :: (MessageHeader -> [SomeDBusValue] -> IO ())
              -> (MessageHeader -> [SomeDBusValue] -> IO a)
              -> TVar AnswerSlots
              -> TVar SignalSlots
              -> TVar PropertySlots
              -> (MessageHeader, [SomeDBusValue])
              -> IO ()
handleMessage handleCall handleSignals answerSlots signalSlots propertySlots
              (header, body) = do
    case messageType header of
        MessageTypeMethodCall -> do
            let hfs = fields header
            logDebug $ "Dispatching method call "
                ++ show (hFPath hfs)
                ++ "; " ++ (maybe "" Text.unpack $ hFInterface hfs)
                ++ "; " ++ (maybe "" Text.unpack $ hFMember hfs)
                ++ ": " ++ show body
            handleCall header body
        MessageTypeMethodReturn -> handleReturn True
        MessageTypeError -> handleError
        MessageTypeSignal -> handleSignal'
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
        Just _ -> handleReturn False
    handleSignal' = do
      _ <- handleSignals header body
      sSlots <- atomically $ readTVar signalSlots
      let fs = fields header
      case () of
         _ | Just iface <- hFInterface fs
           , Just member <- hFMember fs
           , Just path <- hFPath fs
           , Just sender <- hFSender fs
             -> case (iface, member) of
                 ( "org.freedesktop.DBus.Properties"
                  ,"PropertiesChanged")
                     | [DBV pi', DBV uds, DBV invs] <- body
                     , Just propIface <- fromRep =<< castDBV pi' :: Maybe Text
                     , Just updates <- fromRep =<< castDBV uds
                                      :: Maybe (Map Text (DBusValue 'TypeVariant))
                     , Just ivs <- (fromRep =<< castDBV invs :: Maybe [Text])
                       -> handlePropertyUpdates path propIface updates ivs
                 _ -> case filter (match4 ( Match iface
                                          , Match member
                                          , Match path
                                          , Match sender) . fst)
                                    sSlots of
                       handlers@(_:_) ->
                        case listToSomeArguments body of
                         SDBA as -> let sig = SomeSignal $
                                                Signal { signalPath = path
                                                       , signalInterface = iface
                                                       , signalMember = member
                                                       , signalBody = as
                                                       }
                                    in forM_ handlers $ \(_, handler) ->
                                                         handler sig
                       _ -> logDebug $ "Unhandled signal"
                                           ++ show path ++ "/ "
                                           ++ show iface ++ "."
                                           ++ show member ++ " from "
                                           ++ show sender ++ ": "
                                           ++ show body ++ "\n"
           | otherwise -> logDebug $ "Signal is missing header fields:"
                                       ++ show header ++ "; " ++ show body
    match4 (x1, x2, x3, x4) (y1, y2, y3, y4) =
        and $ [ x1 `checkMatch` y1
              , x2 `checkMatch` y2
              , x3 `checkMatch` y3
              , x4 `checkMatch` y4
              ]
    handlePropertyUpdates path iface updates ivs = do
        pSlots <- readTVarIO propertySlots
        let items = Map.toList (Just <$> updates)
                    ++ ((\i -> (i, Nothing)) <$> ivs)
        forM_ items $ \(member, mbV) ->
            case Map.lookup (path, iface, member) pSlots of
                Nothing -> logDebug $ "unexpected property update for "
                                       ++ show path ++" / "
                                       ++ Text.unpack iface ++ "."
                                       ++ Text.unpack member
                Just hs -> do
                    let v = variantToDBV <$> mbV
                    logDebug $ "Recevied property updates " ++ show updates
                               ++ " and invalidated propertied " ++ show ivs
                    forM_ hs $ \h -> forkIO $ h v
    variantToDBV :: DBusValue 'TypeVariant -> SomeDBusValue
    variantToDBV (DBVVariant v) = DBV v

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
        mkReturnMethod s args' = methodReturn s ser sender args'
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
    serial' <- atomically $ dBusCreateSerial conn
    forM_ sigs $ flip emitSignal' conn
    logDebug $ "method call returned " ++ show ret
    case ret of
        Left err -> sendBS conn $ errToErrMessage serial' err
        Right r -> sendBS conn $ mkReturnMethod serial' r
    logDebug "done"
objectRoot _ _ _ _ = return ()

-- | Check whether connection is alive
checkAlive :: DBusConnection -> IO Bool
checkAlive conn = atomically $ readTVar (connectionAliveRef conn)

-- | Wait until connection is closed. The intended use is to keep alive servers
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

type MethodCallHandler = DBusConnection -- ^ Connection that the call was
                                          -- received from. Should be used to
                                          -- return the result or error
                       -> MessageHeader
                       -> [SomeDBusValue]
                       -> IO ()

type SignalHandler = ( DBusConnection
                       -> MessageHeader
                       -> [SomeDBusValue]
                       -> IO ())

-- | General way to connect to a message bus, see 'connectBusWithAuth'.
--
-- Uses the @EXTERNAL@ authentication mechanism.
connectBus :: ConnectionType -- ^ Bus to connect to
           -> MethodCallHandler -- ^ Handler for incoming method calls
           -> SignalHandler  -- ^ Handler for incoming signals
           -> IO DBusConnection
connectBus transport = connectBusWithAuth transport external

-- | General way to connect to a message bus, with a custom authentication
-- method. Takes two callback functions:
--
-- * A 'MethodCallHandler' that is invoked when a method call is received.
--
-- * A SignalHandler that is invoked when a Mesage is received:
connectBusWithAuth :: ConnectionType -- ^ Bus to connect to
                   -> SASL BS.ByteString -- ^ The authentication mechanism
                   -> MethodCallHandler -- ^ Handler for incoming method calls
                   -> SignalHandler  -- ^ Handler for incoming signals
                   -> IO DBusConnection
connectBusWithAuth transport auth handleCalls handleSignals = do
    addressString <- case transport of
        Session -> getEnv "DBUS_SESSION_BUS_ADDRESS"
        System -> do
            fromEnv <- lookupEnv "DBUS_SYSTEM_BUS_ADDRESS"
            case fromEnv of
                Nothing -> return "unix:path=/var/run/dbus/system_bus_socket"
                Just addr -> return addr
        Address addr -> return addr
    debugM "DBus" $ "connecting to " ++ addressString
    mbS <- connectString addressString
    s <- case mbS of
        Nothing -> throwM (CouldNotConnect
                                   "All addresses failed to connect")
        Just s -> return s
    _ <- sendCredentials s
    h <- socketToHandle s ReadWriteMode
    debugM "DBus" $ "Running SASL"
    _ <- runSasl (\bs -> do
                  debugM "DBus.Sasl" $ "C: " ++ show (BS.toLazyByteString bs)
                  BS.hPutBuilder h bs)
            (do
                  bs <- BS.hGetLine h
                  debugM "DBus.Sasl" $ "S: " ++ show bs
                  return bs)
            auth
    serialCounter <- newTVarIO 1
    let getSerial = do
            s' <- readTVar serialCounter
            writeTVar serialCounter (s'+1)
            return s'
    lock <- newTMVarIO $ BS.hPutBuilder h
    answerSlots <- newTVarIO (Map.empty :: AnswerSlots)
    signalSlots <- newTVarIO ([] :: SignalSlots)
    propertySlots <- newTVarIO (Map.empty :: PropertySlots)
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
            atomically $ writeTVar signalSlots []
            atomically $ forM_ (Map.elems slots) $ \s' -> s' . Left $
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
                                    signalSlots
                                    propertySlots
                     ))
            (\e -> print (e :: Ex.SomeException) >> kill >> Ex.throwIO e)
        addFinalizer aliveRef $ killThread handlerThread
        let conn = DBusConnection { dBusCreateSerial = getSerial
                                  , dBusAnswerSlots = answerSlots
                                  , dbusSignalSlots = signalSlots
                                  , dbusPropertySlots = propertySlots
                                  , dBusWriteLock = lock
                                  , dBusConnectionName = ""
                                  , connectionAliveRef = aliveRef
                                  }
        debugM "DBus" $ "hello"
        connName <- hello conn
        debugM "DBus" $ "Done"
        return conn{dBusConnectionName = connName}

-- | Create a simple server that exports @Objects@ and ignores all incoming signals.
--
-- Use the default @EXTERNAL@ authentication mechanism (see 'makeServerWithAuth').
makeServer :: ConnectionType -> Objects -> IO DBusConnection
makeServer transport = makeServerWithAuth transport external

-- | Create a simple server with a custom bus authentication mechanism that
-- exports @Objects@ and ignores all incoming signals.
makeServerWithAuth :: ConnectionType -> SASL BS.ByteString -> Objects -> IO DBusConnection
makeServerWithAuth transport auth objs = do
    connectBusWithAuth transport auth (objectRoot (addIntrospectable objs))
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
