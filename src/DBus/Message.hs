{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Message where

import           Control.Applicative
import           Control.Monad
import qualified Data.Attoparsec.Char8 as AP8
import           Data.Bits
import           Data.Char
import qualified Data.Conduit as C
import qualified Data.Conduit.Serialization.Binary as CSB
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as Text
import           Data.Word
import qualified Data.ByteString as BS

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
                                 , hFErrorname :: Maybe Text.Text
                                 , hFReplyserial :: Maybe Word32
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
    , HeaderFieldErrorName        <$> hFErrorname hfs
    , HeaderFieldReplySerial      <$> hFReplyserial hfs
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
    fromField (HeaderFieldErrorName x)        hf = hf{hFErrorname = Just x}
    fromField (HeaderFieldReplySerial x)      hf = hf{hFReplyserial = Just x}
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
                  }

makeRepresentable ''MessageHeader

messageCall dest path interface member args flags sid =
    let (len, body) = putValues Little args
        sig = Signature $ map (\(DBV v) -> typeOf v) args
        hFields = emptyHeaderFields{ hFPath             = Just path
                                   , hFInterface        = Just interface
                                   , hFMember           = Just member
                                   , hFDestination      = Just dest
                                   , hFMessageSignature = Just sig
                                   }
        header = MessageHeader
                     { endianessFlag = Little
                     , messageType = MethodCall
                     , flags = Flags flags
                     , version = 1
                     , messageLength = len
                     , serial = sid
                     , fields = hFields
                     }
    in snd (putValues Little [DBV $ toRep header]) <> body

peekEndian :: C.MonadThrow m => C.ConduitM BS.ByteString o m Endian
peekEndian = do
    mbBs <- C.await
    case mbBs of
        Nothing -> C.monadThrow $ DBusParseError "Not enough Data"
        Just bs -> do
            e <- case BS.uncons bs of
                Nothing -> C.monadThrow $ DBusParseError "Not enough Data"
                Just (c, _) | chr (fromIntegral c) == 'l' -> return Little
                            | chr (fromIntegral c) == 'b' -> return Big
                            | otherwise -> C.monadThrow . DBusParseError
                                              $ "not an endianness flag: "
                                                ++ (show . chr $ fromIntegral c)
            C.leftover bs
            return e

parseMessages = do
    endian <- peekEndian
    header <- getDBV
