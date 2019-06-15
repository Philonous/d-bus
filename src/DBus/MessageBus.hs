{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module DBus.MessageBus where

import qualified Control.Exception      as Ex
import           Control.Monad.Catch    (MonadThrow, throwM)
import           Control.Monad.IO.Class
import           Control.Monad.Trans    (MonadIO)
import           Data.Default
import           Data.Singletons
import qualified Data.Text              as Text
import           Data.Word

import           DBus.Message
import           DBus.Types
import           DBus.Error

messageBusMethod :: ( MonadIO m
                    , MonadThrow m
                    , Representable args
                    , SingI (RepType args)
                    , SingI (FlattenRepType (RepType args))
                    , Representable ret
                    , SingI (RepType ret)
                    ) =>
                    Text.Text
                 -> args
                 -> DBusConnection
                 -> m ret
messageBusMethod name args con = do
    res <- liftIO $ callMethod "org.freedesktop.DBus"
             (objectPath "/org/freedesktop/DBus")
             "org.freedesktop.DBus" name args [] con
    case res of
        Left e -> liftIO $ Ex.throwIO e
        Right r -> return r

hello :: (MonadIO m, MonadThrow m) => DBusConnection -> m Text.Text
hello = messageBusMethod "Hello" ()

data RequestNameFlag = RequestNameFlag { allowReplacement
                                       , replaceExisting
                                       , doNotQueue :: Bool
                                       }

instance Default RequestNameFlag where
    def = RequestNameFlag False False False

fromRequestNameFlags :: RequestNameFlag -> Word32
fromRequestNameFlags flags' = sum [ fromFlag allowReplacement 0x01
                                  , fromFlag replaceExisting  0x02
                                  , fromFlag doNotQueue       0x04
                                  ]
  where
    fromFlag x n = if x flags' then n else 0

data RequestNameReply = PrimaryOwner
                      | InQueue
                      | Exists
                      | AlreadyOwner

requestName :: (MonadIO m, MonadThrow m) =>
               Text.Text
            -> RequestNameFlag
            -> DBusConnection
            -> m RequestNameReply
requestName name flags' con = do
    reply <- messageBusMethod "RequestName" (name, fromRequestNameFlags flags')
                                            con
    case reply :: Word32 of
        1 -> return PrimaryOwner
        2 -> return InQueue
        3 -> return Exists
        4 -> return AlreadyOwner
        e -> throwM . MarshalError $ "Not a ReqeustName reply: " ++ show e

data ReleaseNameReply = Released
                      | NonExistent
                      | NotOwner

releaseName :: (MonadIO m, MonadThrow m) =>
               Text.Text
            -> DBusConnection
            -> m ReleaseNameReply
releaseName name con = do
        reply <- messageBusMethod "RequestName" name con
        case reply :: Word32 of
            1 -> return Released
            2 -> return NonExistent
            3 -> return NotOwner
            e -> throwM . MarshalError $ "Not a ReleaseName reply: " ++ show e

listQueuedOwners :: (MonadIO m, MonadThrow m) =>
                    Text.Text
                 -> DBusConnection
                 -> m [Text.Text]
listQueuedOwners name = messageBusMethod "ListQueuedOwners" name


listNames :: (MonadIO m, MonadThrow m) => DBusConnection -> m [Text.Text]
listNames = messageBusMethod "ListNames" ()

listActivatableNames :: (MonadIO m, MonadThrow m) =>
                        DBusConnection
                     -> m [Text.Text]
listActivatableNames = messageBusMethod "ListActivatableNames" ()

nameHasOwner :: (MonadIO m, MonadThrow m) => Text.Text -> DBusConnection -> m Bool
nameHasOwner name = messageBusMethod "NameHasOwner" name

data StartServiceResult = StartServiceSuccess
                        | StartServiceAlreadyRunning
                          deriving (Show, Read, Eq)

startServiceByName :: (MonadIO m, MonadThrow m) =>
                      Text.Text
                   -> DBusConnection
                   -> m StartServiceResult
startServiceByName name con = do
    res <- messageBusMethod "StartServiceByName" (name, 0 :: Word32) con
    case (res :: Word32) of
      1 -> return StartServiceSuccess
      2 -> return StartServiceAlreadyRunning
      _ -> throwM $ MarshalError $ "StartServiceByName returned" <> show res

getNameOwner :: (MonadIO m, MonadThrow m) =>
                Text.Text
             -> DBusConnection
             -> m Text.Text
getNameOwner txt = messageBusMethod "GetNameOwner" txt

getConnectionUnixUser :: (MonadIO m, MonadThrow m) =>
                Text.Text
             -> DBusConnection
             -> m Word32
getConnectionUnixUser txt = messageBusMethod "GetConnectionUnixUser" txt

getConnectionProcessID :: (MonadIO m, MonadThrow m) =>
                Text.Text
             -> DBusConnection
             -> m Word32
getConnectionProcessID txt = messageBusMethod "GetConnectionUnixProcessID" txt

getID :: (MonadIO m, MonadThrow m) =>
         DBusConnection
      -> m Text.Text
getID = messageBusMethod "GetId" ()
