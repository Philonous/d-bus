{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module DBus.Message where

import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Reader
import qualified Data.Binary.Get as B
import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Builder as BS
import           Data.Char
import qualified Data.Conduit as C
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Singletons
import           Data.Singletons.Decide
import qualified Data.Text as Text
import           Data.Word
import           System.Mem.Weak
import           Control.Monad.Catch (MonadThrow)
import           Data.Singletons.Prelude.List

import           DBus.Representable
import           DBus.TH
import           DBus.Types
import           DBus.Wire

data MessageType = MessageTypeInvalid
                 | MessageTypeMethodCall
                 | MessageTypeMethodReturn
                 | MessageTypeError
                 | MessageTypeSignal
                 | MessageTypeOther Word8
                 deriving (Eq, Show)

instance Representable MessageType where
    type RepType MessageType = 'DBusSimpleType 'TypeByte
    toRep MessageTypeInvalid      = DBVByte $ 0
    toRep MessageTypeMethodCall   = DBVByte $ 1
    toRep MessageTypeMethodReturn = DBVByte $ 2
    toRep MessageTypeError        = DBVByte $ 3
    toRep MessageTypeSignal       = DBVByte $ 4
    toRep (MessageTypeOther i)    = DBVByte $ fromIntegral i
    fromRep (DBVByte 0) = Just $ MessageTypeInvalid
    fromRep (DBVByte 1) = Just $ MessageTypeMethodCall
    fromRep (DBVByte 2) = Just $ MessageTypeMethodReturn
    fromRep (DBVByte 3) = Just $ MessageTypeError
    fromRep (DBVByte 4) = Just $ MessageTypeSignal
    fromRep (DBVByte i) = Just $ MessageTypeOther $ fromIntegral i

data Flag = NoReplyExpected
          | NoAutoStart
          deriving (Eq, Show)

newtype Flags = Flags [Flag]
              deriving (Show, Eq)

instance Representable Flags where
    type RepType Flags = 'DBusSimpleType 'TypeByte
    toRep (Flags xs) = DBVByte $ foldr (.|.) 0 (map toFlag xs)
      where
        toFlag NoReplyExpected = 0x1
        toFlag NoAutoStart     = 0x2
    fromRep (DBVByte x) = Just . Flags $ fromFlags x

fromFlags :: (Num a, Bits a, Ord a) => a -> [Flag]
fromFlags x | x .&. 0x1 > 0 = NoReplyExpected : fromFlags (x `xor` 0x1)
            | x .&. 0x2 > 0 = NoAutoStart     : fromFlags (x `xor` 0x2)
            | otherwise     = []

instance Representable Signature where
    type RepType Signature = 'DBusSimpleType 'TypeSignature
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
    fromField _                               hf = hf

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
    type RepType Endian = 'DBusSimpleType 'TypeByte
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
methodCall sid dest path interface member args flags' =
    let hFields = emptyHeaderFields{ hFPath             = Just path
                                   , hFInterface        = Just interface
                                   , hFMember           = Just member
                                   , hFDestination      = Just dest
                                   }
        header = MessageHeader
                 { endianessFlag = Little
                 , messageType = MessageTypeMethodCall
                 , flags = Flags flags'
                 , version = 1
                 , messageLength = 0
                 , serial = sid
                 , fields = hFields
                 }
    in serializeMessage header args

mkSignal :: SingI ts => Word32 -> [Flag] -> Signal ts -> BS.Builder
mkSignal sid flags' sig =
    let hFields = emptyHeaderFields { hFPath = Just $ signalPath sig
                                    , hFInterface = Just $ signalInterface sig
                                    , hFMember = Just $ signalMember sig
                                    }
        header = MessageHeader
                 { endianessFlag = Little
                 , messageType = MessageTypeSignal
                 , flags = Flags flags'
                 , version = 1
                 , messageLength = 0
                 , serial = sid
                 , fields = hFields
                 }
    in serializeMessage header (argsToValues . SDBA $ signalBody sig)



methodReturn :: Word32
             -> Word32
             -> Text.Text
             -> SomeDBusArguments
             -> BS.Builder
methodReturn sid rsid dest args =
    let hFields = emptyHeaderFields{ hFDestination = Just dest
                                   , hFReplySerial = Just rsid
                                   }
        header = MessageHeader
                 { endianessFlag = Little
                 , messageType = MessageTypeMethodReturn
                 , flags = Flags []
                 , version = 1
                 , messageLength = 0
                 , serial = sid
                 , fields = hFields
                 }
    in serializeMessage header (argsToValues args)

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
                 , messageType = MessageTypeError
                 , flags = Flags []
                 , version = 1
                 , messageLength = 0
                 , serial = sid
                 , fields = hFields
                 }
    in serializeMessage header (maybe id (\t -> (DBV (DBVString t) :))
                                          text args)


serializeMessage :: MessageHeader -> [SomeDBusValue] -> BS.Builder
serializeMessage head' args =
    let vs = putValues args
        sig = Signature $ map (\(DBV v) -> typeOf v) args
        header len = head' { messageLength = len
                          , fields = (fields head'){hFMessageSignature = Just sig}
                          }
    in runDBusPut Little $ do
        l <- sizeOf 0 1 vs
        putDBV . toRep $ header (fromIntegral l)
        alignPut 8
        vs

getMessage :: B.Get (MessageHeader, [SomeDBusValue])
getMessage = do
    mbEndian <- fromRep <$> (B.lookAhead $ runReaderT getDBV Little)
    endian' <- case mbEndian of
        Nothing -> fail "could not read endiannes flag"
        Just e -> return e
    flip runReaderT endian' $ do
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

sendBS :: DBusConnection -> BS.Builder -> IO ()
sendBS conn bs = do
    write <- atomically . takeTMVar $ dBusWriteLock conn
    write bs
    atomically $ putTMVar (dBusWriteLock conn) write


-- | Asychronously call a method.
--
-- This is the "raw" version of 'callMethod'. It doesn't do argument conversion.
callMethod' :: Text.Text -- ^ Entity to send the message to
           -> ObjectPath -- ^ Object
           -> Text.Text -- ^ Interface
           -> Text.Text -- ^ Member (method) name
           -> [SomeDBusValue] -- ^ Arguments
           -> [Flag] -- ^ Method call flags
           -> DBusConnection -- ^ Connection to send the call over
           -> IO (STM (Either [SomeDBusValue] [SomeDBusValue] ))
callMethod' dest path interface member args flags' conn = do
    serial' <- atomically $ dBusCreateSerial conn
    ref <- newEmptyTMVarIO
    rSlot <- newTVarIO ()
    _ <- mkWeak rSlot (connectionAliveRef conn) Nothing
    _ <- mkWeakTVar rSlot (finalizeSlot serial')
    slot <- atomically $ do
        modifyTVar (dBusAnswerSlots conn) (Map.insert serial' $ putTMVar ref)
        return ref
    let bs = methodCall serial' dest path interface member args flags'
    sendBS conn bs
    return $ readTMVar slot <* readTVar rSlot
  where
    finalizeSlot s = do
        atomically $ modifyTVar (dBusAnswerSlots conn)
                                             (Map.delete s)

toArgs :: Representable args => args -> [SomeDBusValue]
toArgs (arg :: args) =
    let sng = sing :: Sing (RepType args)
        flsng = sFlattenRepType sng
    in withSingI flsng $ argsToValues $ SDBA (flattenRep arg)

-- | Try to convert the response to a method call to a Haskell type
fromResponse :: Representable a =>
                Either [SomeDBusValue] [SomeDBusValue]
             -> Either MethodError a
fromResponse x =
  case fromResponse' x of
    Left e -> Left e
    Right r -> maybe (Left $ MethodSignatureMissmatch [DBV r]) Right $ fromRep r

-- | Try to convert the response to a method call
fromResponse' :: forall (a :: DBusType) .
                 SingI a =>
                 Either [SomeDBusValue] [SomeDBusValue]
              -> Either MethodError (DBusValue a)
fromResponse' (Left e) = Left $ MethodErrorMessage e
fromResponse' (Right rvs) =
    case listToSomeArguments rvs of
        SDBA (r :: DBusArguments ats) ->
            maybe (Left $ MethodSignatureMissmatch rvs) Right
            -- Use fix to access the return type (We only care about the type)
            $ fix $ \(_ :: Maybe (DBusValue ret)) ->
            case sing :: Sing ret of
                STypeStruct ts -> case (r, sing :: Sing ats) of
                    (ArgsNil, SNil) -> Nothing
                    (ArgsCons r' ArgsNil, SCons a SNil) ->
                        case a %~ (sing :: Sing ret) of
                            Proved Refl -> Just r'
                            Disproved _ -> Nothing
                    _ -> withSingI ts
                           $ DBVStruct <$> maybeArgsToStruct r
                STypeUnit -> case r of
                    ArgsNil -> Just DBVUnit
                    _ -> Nothing
                _ -> case (sing :: Sing ats, r) of
                    (SCons at SNil, ArgsCons r' ArgsNil) ->
                        case at %~ (sing :: Sing ret) of
                            Proved Refl -> Just r'
                            Disproved _ -> Nothing
                    _ -> error "fromResponse': impossible case"

-- | Synchronously call a method.
--
-- This is the "cooked" version of callMethod\''. It automatically converts
-- arguments and returns values.
--
-- If the returned value's type doesn't match the expected type a
-- 'MethodSignatureMissmatch' is returned
--
-- You can pass in and extract multiple arguments and return values with types
-- that are represented by DBus Structs (e.g. tuples). More specifically,
-- multiple arguments/return values are handled in the following way:
--
-- * If the method returns multiple values and the RepType of the expected
-- return value is a struct it tries to match up the arguments with the struct
-- fields
--
-- * If the method returns /a single struct/ and the RepType is a struct it will
-- try to match up those two
--
-- * If the RepType of the expected return value is not a struct it will only
-- match if the method returned exactly one value and those two match up (as per
-- normal)
--
-- This means that if the RepType of the expected return value is a struct it
-- might be matched in two ways: Either by the method returning multiple values
-- or the method returning a single struct. At the moment there is no way to
-- exclude either
callMethod :: ( Representable args
              , Representable ret
              ) =>
               Text.Text -- ^ Entity to send the message to
            -> ObjectPath -- ^ Object
            -> Text.Text -- ^ Interface
            -> Text.Text -- ^ Member (method) to call
            -> args -- ^ Arguments
            -> [Flag] -- ^ Method call flags
            -> DBusConnection -- ^ Connection to send the call over
            -> IO (Either MethodError ret)
callMethod dest path interface member (arg :: args) flags' conn = do
    let sng = sing :: Sing (RepType args)
        flsng = sFlattenRepType sng
        args' = withSingI flsng $ argsToValues $ SDBA (flattenRep arg)
    ret <- callMethod' dest path interface member args' flags' conn
    fromResponse <$> atomically ret

callAsync :: forall ret args retList argList.
             ( Representable args
             , Representable ret
             , RepType args ~ FromTypeList argList
             , RepType ret ~ FromTypeList retList
             ) =>
        MethodDescription argList
                          retList
     -> Text.Text
     -> args
     -> [Flag]
     -> DBusConnection
     -> IO (STM (Either MethodError ret))
callAsync md dest args flags' con = do
    res <- callMethod' dest (methodObjectPath md) (methodInterface md)
               (methodMember md) (toArgs args) flags' con
    return $ fromResponse <$> res

call :: ( Representable ret
        , Representable args
        , RepType args ~ FromTypeList argList
        , RepType ret ~ FromTypeList retList
        ) =>
        MethodDescription argList
                          retList
     -> Text.Text
     -> args
     -> [Flag]
     -> DBusConnection
     -> IO (Either MethodError ret)
call md dest args flags' con = atomically =<< callAsync md dest args flags' con
