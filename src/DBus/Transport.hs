{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
module DBus.Transport where

import           Control.Applicative ((<$>))

import qualified Control.Exception as Ex
import           Control.Monad
import           Data.Attoparsec.ByteString as AP
import           Data.Attoparsec.ByteString.Char8 as AP8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import           Data.List as List
import qualified Data.Text as Text
import           Data.Text.Encoding as Text
import           Data.Word
import           Network.Socket

import           DBus.Auth
import           DBus.Error

data UDS = UDSPath BS.ByteString
         | UDSTmpDir BS.ByteString
         | UDSAbstract BS.ByteString
           deriving (Show, Eq)

data TCPFamily = TCPFamilyIPv4
               | TCPFamilyIPv6
                 deriving (Show, Eq)

data TCP = TCP { tcpHost :: Maybe BS.ByteString
               , tcpBind :: Maybe BS.ByteString
               , tcpPort :: Maybe Word16
               , tcpFamily :: Maybe TCPFamily
               } deriving (Show, Eq)

data TransportType = TransportTCP TCP
                   | TransportUnix UDS
                   | OtherTransport BS.ByteString [(BS.ByteString, BS.ByteString)]
                     deriving (Eq, Show)


type GUID = BS.ByteString

withProtocol :: AP.Parser t
             -> ((t, [(BS.ByteString, BS.ByteString)]) -> AP.Parser a)
             -> AP.Parser (Maybe ((Maybe BS.ByteString), a))
withProtocol nameParser f = do
    name <- nameParser
    ((fmap Just $ do
        _ <- AP8.char8 ':'
        pairs <- parsePair `AP.sepBy` (AP8.char8 ',')
        mbGuid <- case List.lookup "guid" pairs of
            Nothing -> return Nothing
            Just g -> case AP.parseOnly parseHexString g of
                Left _ -> fail "could not parse GUID"
                Right r -> return $ Just r
        ret <- f (name, List.filter ((/= "guid").fst) pairs)
        return (mbGuid, ret)
     ) `mplus` (return Nothing))

parsePair :: AP.Parser (BS.ByteString, BS.ByteString)
parsePair = do
    key <- AP8.takeWhile1 (\c -> AP8.isAlpha_ascii c || (c >= '0' && c <= '9'))
    _ <- AP8.char8 '='
    value <- BS.pack <$> AP.many1' valueChar
    return (key, value)
  where
    valueChar = choice [ AP.satisfy $ AP.inClass "0-9A-Za-z_/.\\-"
                       , AP8.char '%' >> parseHexChar
                       ]

parseUnix :: Parser (Maybe (Maybe BS8.ByteString, TransportType))
parseUnix = withProtocol (AP.string "unix") $ \ (_, pairs) -> TransportUnix <$>
    case pairs of
        [("path", p)] -> return $ UDSPath p
        [("tmpdir", p)] -> return $ UDSTmpDir p
        [("abstract", p)] -> return $ UDSAbstract p
        _ -> fail "unix path expects exactly one of path, tmpdir or abstract"

parseTCP :: AP.Parser (Maybe (Maybe GUID, TransportType))
parseTCP = withProtocol (AP.string "tcp") $ \(_, pairs) -> TransportTCP <$>
    foldM addValue (TCP Nothing Nothing Nothing Nothing) pairs
  where
    addValue tcp ("host"   , x) = return $ tcp{tcpHost   = Just x}
    addValue tcp ("bind"   , x) = return $ tcp{tcpBind   = Just x}
    addValue tcp ("port"   , x) = case reads (BS8.unpack x) of
        [(p, "")] -> return tcp{tcpPort   = Just p}
        _ -> fail "could not read port"
    addValue tcp ("family" , x) = case x of
        "ipv4" -> return $ tcp{tcpFamily = Just TCPFamilyIPv4}
        "ipv6" -> return $ tcp{tcpFamily = Just TCPFamilyIPv6}
        _ -> fail "unknown family"
    addValue _ _ = fail "unknown key"

parseOtherTransport :: AP.Parser (Maybe (Maybe GUID, TransportType))
parseOtherTransport =
    withProtocol (AP.takeWhile1 $ AP.inClass "a-zA-Z0-9-")
    $ \(name, pairs) -> return $ OtherTransport name pairs

parseMaybe :: MonadPlus m => Maybe a -> m a
parseMaybe Nothing = mzero
parseMaybe (Just x) = return x

parseTransport :: Parser (Maybe GUID, TransportType)
parseTransport = AP.choice [ parseMaybe =<< parseTCP
                           , parseMaybe =<< parseUnix
                           , parseMaybe =<< parseOtherTransport
                           ]

parseTransports :: AP.Parser [(Maybe GUID, TransportType)]
parseTransports = parseTransport `AP.sepBy` AP8.char8 ';'

connectTcp :: TCP -> IO Socket
connectTcp tcp | Just host <- tcpHost tcp
               , Just port <- tcpPort tcp
               , port > 0
               = do
    let family = case tcpFamily tcp of
            Nothing -> AF_UNSPEC
            Just TCPFamilyIPv4 -> AF_INET
            Just TCPFamilyIPv6 -> AF_INET6
    addrInfo <- withPort port <$> getAddrInfo (Just $ defaultHints{ addrFamily = family} )
                                              (Just . Text.unpack . Text.decodeUtf8 $ host)
                                              Nothing
    case addrInfo of
        (ai : _) -> Ex.catch
            (Ex.bracketOnError (socket (addrFamily ai)
                                       (addrSocketType ai)
                                       (addrProtocol ai))
                               close
                               $ \s -> do
                                   connect s $ addrAddress ai
                                   return s
            )
            (\(_ :: Ex.SomeException) ->
                       Ex.throwIO $ CouldNotConnect "Could not connect")

        _ -> Ex.throwIO $ CouldNotConnect "Host not found"
  where
    withPort p = fmap (addrInfoWithPort $ fromIntegral p)
    addrInfoWithPort p ai = ai { addrAddress = sockAddrWithPort p (addrAddress ai) }

    sockAddrWithPort p (SockAddrInet _ addr) = SockAddrInet p addr
    sockAddrWithPort p (SockAddrInet6 _ info addr sid) = SockAddrInet6 p info addr sid
    sockAddrWithPort _ x = x

connectTcp _ = Ex.throwIO $ CouldNotConnect "TCP method does not specify necessary data"

connectUnix :: UDS -> IO Socket
connectUnix unix = do
    s <- socket AF_UNIX Stream defaultProtocol
    addr <- case unix of
        UDSPath p -> return . SockAddrUnix . Text.unpack $ Text.decodeUtf8 p
        UDSAbstract p -> return . SockAddrUnix . ('\0':)
                         . Text.unpack $ Text.decodeUtf8 p
        UDSTmpDir _ -> Ex.throwIO $ CouldNotConnect "Can not connecto to Tmp dir"
    Ex.catch (connect s addr)
             (\(e:: Ex.SomeException) ->
               Ex.throwIO . CouldNotConnect $ "Error connecting to Unix Socket: "
                                               ++ show e)
    return s

connectTransport :: TransportType -> IO Socket
connectTransport (TransportTCP tcp) = connectTcp tcp
connectTransport (TransportUnix unix) = connectUnix unix
connectTransport (OtherTransport name _) = Ex.throwIO . CouldNotConnect $
                                      "Transport method " ++ show name
                                      ++ "not implemented"

connectString :: String -> IO (Maybe Socket)
connectString s = case AP.parseOnly parseTransports
                            (Text.encodeUtf8 . Text.pack $ s) of
                    Left _ -> return $ Nothing
                    Right transports -> go transports
  where
    go ((_, t) : ts) = do
        mbS <- Ex.try $ connectTransport t
        case mbS of
            Left (_ :: DBusError) -> go ts
            Right s' -> return $ Just s'
    go [] = return Nothing
