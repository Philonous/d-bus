{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module DBus.Message where

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Reader
import qualified Data.Attoparsec.ByteString.Char8 as AP8
import qualified Data.Binary.Get as B
import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Builder as BS
import           Data.Char
import qualified Data.Conduit as C
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Singletons
import qualified Data.Text as Text
import           Data.Word
import           System.Mem.Weak
import           Control.Monad.Catch (MonadThrow, throwM)

import           DBus.Error
import           DBus.Representable
import           DBus.TH
import           DBus.Types
import           DBus.Wire

data MessageType = Invalid
                 | MethodCall
                 | MethodReturn
                 | Error
                 | Signal
                 | Other Word8
                 deriving (Eq, Show)

instance Representable MessageType where
    type RepType MessageType = 'DBusSimpleType TypeByte
    toRep Invalid      = DBVByte $ 0
    toRep MethodCall   = DBVByte $ 1
    toRep MethodReturn = DBVByte $ 2
    toRep Error        = DBVByte $ 3
    toRep Signal       = DBVByte $ 4
    toRep (Other i)    = DBVByte $ fromIntegral i
    fromRep (DBVByte 0) = Just $ Invalid
    fromRep (DBVByte 1) = Just $ MethodCall
    fromRep (DBVByte 2) = Just $ MethodReturn
    fromRep (DBVByte 3) = Just $ Error
    fromRep (DBVByte 4) = Just $ Signal
    fromRep (DBVByte i) = Just $ Other $ fromIntegral i

data Flag = NoReplyExpected
          | NoAutoStart
          deriving (Eq, Show)

newtype Flags = Flags [Flag]
              deriving (Show, Eq)

instance Representable Flags where
    type RepType Flags = 'DBusSimpleType TypeByte
    toRep (Flags xs) = DBVByte $ foldr (.|.) 0 (map toFlag xs)
      where
        toFlag NoReplyExpected = 0x1
        toFlag NoAutoStart     = 0x2
    fromRep (DBVByte x) = Just . Flags $ fromFlags x

fromFlags x | x .&. 0x1 > 0 = NoReplyExpected : fromFlags (x `xor` 0x1)
            | x .&. 0x2 > 0 = NoAutoStart     : fromFlags (x `xor` 0x2)
            | otherwise     = []

instance Representable Signature where
    type RepType Signature = 'DBusSimpleType TypeSignature
    toRep (Signature ts) = DBVSignature ts
    fromRep (DBVSignature ts) = Just $ Signature ts

data HeaderFields = HeaderFields { hFPath :: Maybe ObjectPath
                                 , hFInterface :: Maybe Text.Text
                                 , hFMember :: Maybe Text.Text
                                 , hFErrorName :: Maybe Text.Text
                                 , hFReplySerial :: Maybe Word32
                                 , hFDestination :: Maybe Text.Text
                                 , hFSender :: Maybe Text.Text
                                 , hFMessageSignature :: Maybe Signature
                                 , hFUnixfds :: Maybe Word32
                                 } deriving (Show, Eq)

instance Representable HeaderFields where
    type RepType HeaderFields = RepType [HeaderField]
    toRep = toRep . toFields
    fromRep =  fmap fromFields . fromRep

emptyHeaderFields :: HeaderFields
emptyHeaderFields = HeaderFields Nothing Nothing Nothing Nothing Nothing
                                  Nothing Nothing Nothing Nothing

toFields :: HeaderFields -> [HeaderField]
toFields hfs = catMaybes
    [ HeaderFieldPath             <$> hFPath hfs
    , HeaderFieldInterface        <$> hFInterface hfs
    , HeaderFieldMember           <$> hFMember hfs
    , HeaderFieldErrorName        <$> hFErrorName hfs
    , HeaderFieldReplySerial      <$> hFReplySerial hfs
    , HeaderFieldDestination      <$> hFDestination hfs
    , HeaderFieldSender           <$> hFSender hfs
    , HeaderFieldMessageSignature <$> hFMessageSignature hfs
    , HeaderFieldUnixFDs          <$> hFUnixfds hfs
    ]

fromFields :: [HeaderField] -> HeaderFields
fromFields fs = foldr fromField emptyHeaderFields fs
  where
    fromField (HeaderFieldPath x)             hf = hf{hFPath = Just x}
    fromField (HeaderFieldInterface x)        hf = hf{hFInterface = Just x}
    fromField (HeaderFieldMember x)           hf = hf{hFMember = Just x}
    fromField (HeaderFieldErrorName x)        hf = hf{hFErrorName = Just x}
    fromField (HeaderFieldReplySerial x)      hf = hf{hFReplySerial = Just x}
    fromField (HeaderFieldDestination x)      hf = hf{hFDestination = Just x}
    fromField (HeaderFieldSender x)           hf = hf{hFSender = Just x}
    fromField (HeaderFieldMessageSignature x) hf = hf{hFMessageSignature = Just x}
    fromField (HeaderFieldUnixFDs     x)      hf = hf{hFUnixfds = Just x}
    fromFields _                              hf = hf

data HeaderField = HeaderFieldInvalid
                 | HeaderFieldPath ObjectPath
                 | HeaderFieldInterface Text.Text
                 | HeaderFieldMember Text.Text
                 | HeaderFieldErrorName Text.Text
                 | HeaderFieldReplySerial Word32
                 | HeaderFieldDestination Text.Text
                 | HeaderFieldSender Text.Text
                 | HeaderFieldMessageSignature Signature
                 | HeaderFieldUnixFDs Word32
                   deriving (Show, Eq)

makeRepresentable ''HeaderField

instance Representable Endian where
    type RepType Endian = 'DBusSimpleType TypeByte
    toRep Little = DBVByte $ fromIntegral $ ord 'l'
    toRep Big    = DBVByte $ fromIntegral $ ord 'b'
    fromRep (DBVByte x) = case chr (fromIntegral x) of
        'l' -> Just Little
        'b' -> Just Big
        _ -> Nothing

data MessageHeader =
    MessageHeader { endianessFlag :: Endian
                  , messageType :: MessageType
                  , flags :: Flags
                  , version :: Word8
                  , messageLength :: Word32
                  , serial :: Word32
                  , fields :: HeaderFields
                  } deriving (Show, Eq)

makeRepresentable ''MessageHeader

methodCall :: Word32
           -> Text.Text
           -> ObjectPath
           -> Text.Text
           -> Text.Text
           -> [SomeDBusValue]
           -> [Flag]
           -> BS.Builder
methodCall sid dest path interface member args flags =
    let hFields = emptyHeaderFields{ hFPath             = Just path
                                   , hFInterface        = Just interface
                                   , hFMember           = Just member
                                   , hFDestination      = Just dest
                                   }
        header = MessageHeader
                 { endianessFlag = Little
                 , messageType = MethodCall
                 , flags = Flags flags
                 , version = 1
                 , messageLength = 0
                 , serial = sid
                 , fields = hFields
                 }
    in serializeMessage header args

methodReturn :: Word32
             -> Word32
             -> Text.Text
             -> [SomeDBusValue]
             -> BS.Builder
methodReturn sid rsid dest args =
    let hFields = emptyHeaderFields{ hFDestination = Just dest
                                   , hFReplySerial = Just rsid
                                   }
        header = MessageHeader
                 { endianessFlag = Little
                 , messageType = MethodReturn
                 , flags = Flags []
                 , version = 1
                 , messageLength = 0
                 , serial = sid
                 , fields = hFields
                 }
    in serializeMessage header args

errorMessage :: Word32
             -> Maybe Word32
             -> Text.Text
             -> Text.Text
             -> Maybe Text.Text
             -> [SomeDBusValue]
             -> BS.Builder
errorMessage sid rsid dest name text args =
    let hFields = emptyHeaderFields{ hFDestination = Just dest
                                   , hFErrorName   = Just name
                                   , hFReplySerial = rsid
                                   }
        header = MessageHeader
                 { endianessFlag = Little
                 , messageType = Error
                 , flags = Flags []
                 , version = 1
                 , messageLength = 0
                 , serial = sid
                 , fields = hFields
                 }
    in serializeMessage header (maybe id (\t -> (DBV (DBVString t) :))
                                          text args)


serializeMessage head args =
    let vs = putValues args
        sig = Signature $ map (\(DBV v) -> typeOf v) args
        header len = head { messageLength = len
                          , fields = (fields head){hFMessageSignature = Just sig}
                          }
    in runDBusPut Little $ do
        l <- sizeOf 0 1 vs
        putDBV . toRep $ header (fromIntegral l)
        alignPut 8
        vs

getMessage = do
    mbEndian <- fromRep <$> (B.lookAhead $ runReaderT getDBV Little)
    endian <- case mbEndian of
        Nothing -> fail "could not read endiannes flag"
        Just e -> return e
    flip runReaderT endian $ do
        mbHeader <- fromRep <$> getDBV
        header <- case mbHeader of
            Nothing -> fail "Header has wrong type"
            Just h -> return h
        alignGet 8
        args <- case hFMessageSignature $ fields header of
            Nothing -> return []
            Just (Signature sigs) -> forM sigs $ \t ->
                getDBVByType t
        return (header, args)

parseMessages :: MonadThrow m =>
                 C.ConduitM BS.ByteString (MessageHeader, [SomeDBusValue]) m b
parseMessages = forever $ C.yield =<< sinkGet getMessage

sendBS conn bs = do
    write <- atomically . takeTMVar $ dBusWriteLock conn
    write bs
    atomically $ putTMVar (dBusWriteLock conn) write


-- | Asychronously call a method. Returns an STM action that waits for the
-- returned value.
callMethod :: Text.Text -- ^ Entity to send the message to
           -> ObjectPath -- ^ Object
           -> Text.Text -- ^ Interface
           -> Text.Text -- ^ Member (method) name
           -> [SomeDBusValue] -- ^ Arguments
           -> [Flag] -- ^ Method call flags
           -> DBusConnection -- ^ Connection to send the call over
           -> IO (STM (Either [SomeDBusValue] SomeDBusValue))
callMethod dest path interface member args flags conn = do
    serial <- atomically $ dBusCreateSerial conn
    ref <- newEmptyTMVarIO
    rSlot <- newTVarIO ()
    mkWeak rSlot (connectionAliveRef conn) Nothing
    addFinalizer rSlot (finalizeSlot serial)
    slot <- atomically $ do
        modifyTVar (dBusAnswerSlots conn) (Map.insert serial $ putTMVar ref)
        return ref
    let bs = methodCall serial dest path interface member args flags
    sendBS conn bs
    return $ readTMVar slot <* readTVar rSlot
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
callMethod' :: (SingI (RepType a), Representable a, MonadThrow m, MonadIO m) =>
               Text.Text -- ^ Entity to send the message to
            -> ObjectPath -- ^ Object
            -> Text.Text -- ^ Interface
            -> Text.Text -- ^ Member (method) to call
            -> [SomeDBusValue] -- ^ Arguments
            -> [Flag] -- ^ Method call flags
            -> DBusConnection -- ^ Connection to send the call over
            -> m a
callMethod' dest path interface member args flags conn = do
    ret <- liftIO . getAnswer
               $ callMethod dest path interface member args flags conn
    case ret of
        Left e -> throwM $ MethodErrorMessage e
        Right r -> case fromRep =<< dbusValue r of
            Nothing -> throwM $ MethodSignatureMissmatch r
            Just x -> return x
