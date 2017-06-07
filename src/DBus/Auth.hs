{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module DBus.Auth where

import           Control.Monad.Except
import           Control.Monad.Free
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.Attoparsec.ByteString.Char8 as AP8
import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Builder as BS
import           Data.Monoid
import           Data.Word
import           Numeric
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

type Mechanism = BS.ByteString

type InitialResponse = BS.ByteString

data ServerMessage = SMRejected [BS.ByteString]
                   | SMOK BS.ByteString
                   | SMError BS.ByteString
                   | SMAgreeUnixFD
                   | SMData BS.ByteString
                     deriving (Show)

space :: AP.Parser BS.ByteString -> AP.Parser BS.ByteString
space x = (AP8.char8 ' ' >> x) `mplus` (return "")


parseHexString :: AP.Parser BS.ByteString
parseHexString = space $ (BS.pack <$> AP.many' parseHexChar)

parseHexChar :: AP.Parser Word8
parseHexChar = do
    hi <- fromHex =<< AP.satisfy isHexDigit
    lo <- fromHex =<< AP.satisfy isHexDigit
    return $ (hi `shiftL` 4) .|. lo
  where
    isHexDigit w = (w >= 48 && w <= 57) ||
                   (w >= 97 && w <= 102)
    fromHex w | w >= 48 && w <= 57  =  return $ fromIntegral (w - 48)
              | w >= 97             =  return $ fromIntegral (w - 87)
    fromHex w = fail $ "Not a hex character: " <> show w



encodeHexString :: BS.ByteString -> BS.Builder
encodeHexString bs = mconcat . map hexifyChar $ BS.unpack bs
  where
    hexifyChar c = case showHex c "" of
        [x] -> BS.char8 '0' <> BS.char8 x
        [x,y] -> BS.char8 x <> BS.char8 y
        _ -> error "encodeHexString : unexpected number of chars"

parseLine :: BS.ByteString
          -> (t -> a)
          -> AP.Parser t
          -> AP.Parser a
parseLine command cons args = do
    _ <- AP.string command
    res <- args
    _ <- AP.string "\r"
    return $ cons res

parseWords :: AP.Parser [BS.ByteString]
parseWords = (AP8.char8 ' ' >> (parseWord `AP.sepBy` (AP8.char8 ' ')))
             `mplus` (return [])

parseWord :: AP.Parser BS.ByteString
parseWord = space $ AP8.takeWhile1 wordChar
  where
    wordChar c = c /= ' ' && c /= '\r' && c /= '\n'

restOfLine :: AP.Parser BS.ByteString
restOfLine = space $ AP8.takeWhile (/= '\r')

parseServerLine :: AP.Parser ServerMessage
parseServerLine = AP.choice
                  [ parseLine "REJECTED" SMRejected parseWords
                  , parseLine "OK" SMOK parseHexString
                  , parseLine "ERROR" SMError restOfLine
                  , parseLine "AGREE_UNIX_FD" (const SMAgreeUnixFD) (return ())
                  , parseLine "DATA" SMData parseHexString
                  ]


data ClientMessage = CMAuth Mechanism InitialResponse
                   | CMCancel
                   | CMBegin
                   | CMData BS.ByteString
                   | CMError BS.ByteString
                   | CMNegotiateUnixFD
                     deriving (Show)


serializeLine :: BS.ByteString -> BS.Builder -> BS.Builder
serializeLine command rest =
    BS.byteString command <> BS.char8 ' ' <> rest <> BS.byteString "\r\n"


serializeCMessage :: ClientMessage -> BS.Builder
serializeCMessage (CMAuth mechanism response) =
    serializeLine "AUTH" $ if BS.null mechanism
                           then mempty
                           else BS.byteString mechanism
                                <> if BS.null response
                                   then mempty
                                   else BS.char8 ' ' <> BS.byteString response
serializeCMessage CMCancel   = serializeLine "CANCEL" mempty
serializeCMessage CMBegin    = serializeLine "BEGIN" mempty
serializeCMessage (CMData d) = serializeLine "DATA" (encodeHexString d)
serializeCMessage (CMError d) = serializeLine "ERROR" (BS.byteString d)
serializeCMessage CMNegotiateUnixFD =
    serializeLine "AGREE_UNIX_FD" mempty

data SASLF a = Send ClientMessage a
             | Recv (ServerMessage -> a)
             deriving (Functor)

newtype SASL a = SASL {unSASL :: ExceptT String (Free SASLF) a}
               deriving (Functor, Applicative, Monad)

instance MonadError String SASL where
    throwError = SASL . throwError
    catchError (SASL m) f = SASL $ catchError m (unSASL . f)

saslSend :: ClientMessage -> SASL ()
saslSend x = SASL . lift $ Free (Send x (return ()))

saslRecv :: SASL ServerMessage
saslRecv = SASL . lift $ Free (Recv $ return )

expectData :: SASL BS.ByteString
expectData = do
    r <- saslRecv
    case r of
        SMData x -> return x
        e -> throwError $ "Expected DATA but got " ++ show e

expectOK :: SASL BS.ByteString
expectOK = do
    r <- saslRecv
    case r of
        SMOK x -> return x
        e -> throwError $ "Expected OK but got " ++ show e

runSasl :: Monad m =>
           (BS.Builder -> m a)
        -> m BS.ByteString
        -> SASL b
        -> m (Either String b)
runSasl snd' rcv' (SASL s) = do
    let sd = snd' . serializeCMessage
        rcv = do
            bs <- rcv'
            case AP.parseOnly parseServerLine bs of
                Left e -> return .
                          SMError . Text.encodeUtf8 . Text.pack $
                          "Could not parse server message" ++ show bs
                                     ++ ": " ++ show e
                Right r -> return r
    return ()
    res <- go sd rcv (runExceptT s)
    case res of
        Left e -> do
            _ <- sd CMCancel
            return $ Left e
        Right r -> return $ Right r
  where
    go  _   _   (Pure x) = return x
    go  sd rcv (Free (Send x f)) = sd x >> go sd rcv f
    go  sd rcv (Free (Recv f  )) = rcv >>=  go sd rcv . f

sasl :: SASL ServerMessage
sasl = do
    saslSend (CMAuth "" "")
    saslRecv

external :: SASL BS.ByteString
external = do
    saslSend (CMAuth "EXTERNAL" "")
    "" <- expectData
    saslSend (CMData "")
    ok <- expectOK
    saslSend CMBegin
    return ok

anonymous :: SASL BS.ByteString
anonymous = do
    saslSend (CMAuth "ANONYMOUS" "")
    ok <- expectOK
    saslSend CMBegin
    return ok
