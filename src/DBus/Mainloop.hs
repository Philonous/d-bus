{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternGuards #-}

module DBus.Mainloop where

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
import           System.Mem.Weak

import           Data.Attoparsec as AP
import           Data.List (intercalate)
import           Data.Monoid
import           Numeric

import           DBus.Auth
import           DBus.Transport
import           DBus.Wire
import           DBus.Message
import           DBus.Types
import           DBus.Object

data MethodError = MethodErrorMessage [SomeDBusValue]
                 | MethodSignatureMissmatch SomeDBusValue
                   deriving (Show, Typeable)

instance Ex.Exception MethodError

listNames :: DBusConnection -> IO [Text.Text]
listNames = callMethod' "org.freedesktop.DBus"
                       (objectPath "/org/freedesktop/DBus")
                       "org.freedesktop.DBus" "ListNames" [] []

hello :: DBusConnection -> IO Text.Text
hello = callMethod' "org.freedesktop.DBus" (objectPath "/org/freedesktop/DBus")
                         "org.freedesktop.DBus" "Hello" [] []

type Serial = Word32
type Slot = Either [SomeDBusValue] SomeDBusValue -> STM ()
type AnswerSlots = Map.Map Serial Slot

data DBusConnection =
    DBusConnection
        { dBusCreateSerial :: STM Serial
        , dBusAnswerSlots :: TVar AnswerSlots
        , dBusWriteLock :: TMVar (BS.Builder -> IO ())
        , dBusConnectionName :: Text.Text
        , connectionAliveRef :: TVar Bool
        }

sendBS conn bs = do
    write <- atomically . takeTMVar $ dBusWriteLock conn
    write bs
    atomically $ putTMVar (dBusWriteLock conn) write

handleMessage handleCall answerSlots (header, body) =
    case messageType header of
        MethodCall -> handleCall header body
        MethodReturn -> handleReturn True
        Error -> handleError
        Signal -> handleSignal
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
    handleSignal = return ()

-- | Asychronously call a method. Returns an STM action that waits for the
-- returned value
callMethod  :: Text.Text
            -> ObjectPath
            -> Text.Text
            -> Text.Text
            -> [SomeDBusValue]
            -> [Flag]
            -> DBusConnection
            -> IO (STM (Either [SomeDBusValue] SomeDBusValue))
callMethod dest path interface member args flags conn = do
    serial <- atomically $ dBusCreateSerial conn
    ref <- newEmptyTMVarIO
    mkWeak ref (connectionAliveRef conn) Nothing
    rSlot <- newTVarIO ()
    addFinalizer rSlot (finalizeSlot serial)
    slot <- atomically $ do
        modifyTVar (dBusAnswerSlots conn) (Map.insert serial (putTMVar ref))
        return ref
    let bs = methodCall serial dest path interface member args flags
    sendBS conn bs
    return $ (readTMVar slot <* readTVar rSlot)
  where
    finalizeSlot s = do
        atomically $ modifyTVar (dBusAnswerSlots conn)
                                             (Map.delete s)

-- | Wait for the answer of a method call
getAnswer :: IO (STM b) -> IO b
getAnswer = (atomically =<<)

-- | Synchronously call a method. Returned errors are thrown as 'MethodError's.
-- If the returned value's type doesn't match the expected type a
-- 'MethodSignatureMissmatch' is thrown.
callMethod' :: (SingI (RepType a), Representable a, C.MonadThrow m, MonadIO m) =>
               Text.Text
            -> ObjectPath
            -> Text.Text
            -> Text.Text
            -> [SomeDBusValue]
            -> [Flag]
            -> DBusConnection
            -> m a
callMethod' dest path interface member args flags conn = do
    ret <- liftIO . getAnswer
           $ callMethod dest path interface member args flags conn
    case ret of
        Left e -> C.monadThrow $  MethodErrorMessage e
        Right r -> case fromRep =<< (dbusValue r) of
            Nothing -> C.monadThrow $ MethodSignatureMissmatch r
            Just x -> return x

-- | Create a new connection to a message bus
--newConnection :: String -> IO DBusConnection
newConnection transportString handleCalls = do
    Just s <- connectString transportString
    send s "\0"
    h <- socketToHandle s ReadWriteMode
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
        putStrLn "finalizing"
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
        handlerThread <- forkIO $ (do
            CB.sourceHandle h
                C.$= parseMessages
                C.$$ (C.awaitForever $ liftIO .
                      handleMessage (handleCalls conn') answerSlots))
                         `Ex.finally` kill
        addFinalizer aliveRef $ killThread handlerThread
        let conn = DBusConnection { dBusCreateSerial = getSerial
                                  , dBusAnswerSlots = answerSlots
                                  , dBusWriteLock = lock
                                  , dBusConnectionName = ""
                                  , connectionAliveRef = aliveRef
                                  }
        connName <- hello conn
        return conn{dBusConnectionName = connName}

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
