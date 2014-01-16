{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module DBus.Wire where


import           Control.Applicative ((<$>), (<*>))
import           Control.Monad
import           Control.Monad.RWS
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans
import qualified Data.Binary.Get as B
import qualified Data.Binary.IEEE754 as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Builder as BS
import           Data.Int
import           Data.Singletons
import           Data.Singletons.List
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Word

import           DBus.Types
import           DBus.Signature

fromEnum' :: (Enum a, Num c) => a -> c
fromEnum' = fromIntegral . fromEnum

toEnum' :: (Enum c, Integral a) => a -> c
toEnum' = toEnum . fromIntegral

data Endian = Little | Big

type DBusPut a = RWS Endian BS.Builder Int a

endian :: (a -> BS.Builder)
       -> (a -> BS.Builder)
       -> a
       -> DBusPut ()
endian l b x = do
    e <- ask
    case e of
        Little -> tell $ l x
        Big -> tell $ b x

putSize :: MonadState Int m => Int -> m ()
putSize i = modify (+i)

alignPut :: Int -> DBusPut ()
alignPut bytes = do
    s <- get
    let align = (bytes - (s `mod` bytes))
    replicateM align . tell $ BS.word8 0
    putSize align

sizeOf :: DBusPut a -> DBusPut Int
sizeOf x = do
    s <- ask
    return $ fst (execRWS x s 0)

bytes :: (a -> BS.Builder)
      -> (a -> BS.Builder)
      -> Int
      -> a
      -> DBusPut ()
bytes l b bytes x = alignPut bytes >> endian l b x >> putSize bytes

putWord8 :: Word8 -> DBusPut ()
putWord8 x = (tell $ BS.word8 x) >> putSize 1

putWord16 :: Word16 -> DBusPut ()
putWord16 = bytes BS.word16LE BS.word16BE 2

putWord32 :: Word32 -> DBusPut ()
putWord32 = bytes BS.word32LE BS.word32BE 4

putWord64 :: Word64 -> DBusPut ()
putWord64 = bytes BS.word64LE BS.word64BE 8

putInt8 :: Int8 -> DBusPut ()
putInt8 = putWord8 . fromIntegral

putInt16 :: Int16 -> DBusPut ()
putInt16 = putWord16 . fromIntegral

putInt32 :: Int32 -> DBusPut ()
putInt32 = putWord32 . fromIntegral

putInt64 :: Int64 -> DBusPut ()
putInt64 = putWord64 . fromIntegral

putDouble :: Double -> DBusPut ()
putDouble = bytes BS.doubleLE BS.doubleBE 8

putByteString :: BS.ByteString -> DBusPut ()
putByteString x = do
    tell $ BS.byteString x
    putSize (BS.length x)

putText :: Text.Text -> DBusPut ()
putText t = do
    let bs = Text.encodeUtf8 t
    putWord32 . fromIntegral $ BS.length bs
    putByteString bs
    putWord8 0

putObjectPath :: ObjectPath -> DBusPut ()
putObjectPath o = putText $ objectPathToText o

putSignatures s = do
    let bs = toSignatures s
    putWord8 (fromIntegral $ BS.length bs)
    putByteString bs
    putWord8 0

putDBV :: DBusValue t -> DBusPut ()
putDBV (DBVByte       x) = putWord8 x
putDBV (DBVBool       x) = putWord32 $ fromEnum' x
putDBV (DBVInt16      x) = putInt16 x
putDBV (DBVUInt16     x) = putWord16 x
putDBV (DBVInt32      x) = putInt32 x
putDBV (DBVUInt32     x) = putWord32 x
putDBV (DBVInt64      x) = putInt64 x
putDBV (DBVUint64     x) = putWord64 x
putDBV (DBVDouble     x) = putDouble x
putDBV (DBVUnixFD     x) = putWord32 x
putDBV (DBVString     x) = putText x
putDBV (DBVObjectPath x) = putObjectPath x
putDBV (DBVSignature  x) = putSignatures x

putDBV (DBVVariant    x) = putSignatures [typeOf x]
                           >> putDBV x
putDBV (DBVArray      x) = let content = mapM_ putDBV x in do
    putWord32 . fromIntegral =<< sizeOf content
    content
putDBV (DBVByteArray  x) = do
    putWord32 . fromIntegral $ BS.length x
    putByteString x
putDBV (DBVStruct     x) = do
    alignPut 8
    putStruct x
putDBV (DBVDict       x) =
    let content = forM_ x $ \(k,v) -> do
            alignPut 8
            putDBV k
            putDBV v
    in do
        putWord32 . fromIntegral =<< sizeOf content
        content

putStruct :: DBusStruct a -> DBusPut ()
putStruct (StructSingleton v) = putDBV v
putStruct (StructCons v vs ) = putDBV v >> putStruct vs

-----------------------------------
-- Get
-----------------------------------

type DBusGet a = ReaderT Endian B.Get a

getEndian :: B.Get a -> B.Get a -> DBusGet a
getEndian l b = do
    e <- ask
    case e of
        Little ->  lift $ l
        Big ->  lift $ b

alignGet :: Int -> DBusGet ()
alignGet bytes = do
    s <- fromIntegral <$> lift B.bytesRead
    let align = (bytes - (s `mod` bytes))
    lift $ B.skip align

getting :: B.Get a -> B.Get a -> Int -> DBusGet a
getting l b i = do
    alignGet i
    getEndian l b

getWord8 :: DBusGet Word8
getWord8 = getting B.getWord8 B.getWord8 1

getWord16 :: DBusGet Word16
getWord16 = getting B.getWord16le B.getWord16be 2

getWord32 :: DBusGet Word32
getWord32 = getting B.getWord32le B.getWord32be 4

getWord64 :: DBusGet Word64
getWord64 = getting B.getWord64le B.getWord64be 8

getInt16 :: DBusGet Int16
getInt16 = fromIntegral <$> getWord16

getInt32 :: DBusGet Int32
getInt32 = fromIntegral <$> getWord32

getInt64 :: DBusGet Int64
getInt64 = fromIntegral <$> getWord64

getDouble :: DBusGet Double
getDouble = getting B.getFloat64le B.getFloat64be 8

getText :: DBusGet Text.Text
getText = do
    len <- getWord32
    bs <- lift $ B.getByteString (fromIntegral len)
    guard . (== 0) =<< getWord8
    case Text.decodeUtf8' bs of
        Left _ -> mzero
        Right txt -> return txt

getBool :: DBusGet Bool
getBool = toEnum' <$> getWord32

getSignatures = do
    len <- getWord8
    bs <- lift $ B.getByteString (fromIntegral len)
    guard . (0 ==) =<< getWord8
    case parseSigs bs of
         Nothing -> mzero
         Just s -> return s

getByteString = lift . B.getByteString

alignment :: DBusType -> Int
alignment (DBusSimpleType TypeByte    ) = 1
alignment (DBusSimpleType TypeBoolean ) = 4
alignment (DBusSimpleType TypeInt16   ) = 2
alignment (DBusSimpleType TypeUInt16  ) = 2
alignment (DBusSimpleType TypeInt32   ) = 4
alignment (DBusSimpleType TypeUInt32  ) = 4
alignment (DBusSimpleType TypeInt64   ) = 8
alignment (DBusSimpleType TypeUInt64  ) = 8
alignment (DBusSimpleType TypeDouble  ) = 8
alignment (DBusSimpleType TypeUnixFD  ) = 4
alignment (DBusSimpleType TypeString  ) = 4
alignment (DBusSimpleType TypeObjectPath) = 4
alignment (DBusSimpleType TypeSignature) = 1
alignment TypeVariant = 1
alignment (TypeArray _) = 4
alignment (TypeStruct _) = 8

getDBV :: Sing t -> DBusGet (DBusValue t)
getDBV (SDBusSimpleType STypeByte    ) = DBVByte    <$> getWord8
getDBV (SDBusSimpleType STypeBoolean ) = DBVBool    <$> getBool
getDBV (SDBusSimpleType STypeInt16   ) = DBVInt16   <$> getInt16
getDBV (SDBusSimpleType STypeUInt16  ) = DBVUInt16  <$> getWord16
getDBV (SDBusSimpleType STypeInt32   ) = DBVInt32   <$> getInt32
getDBV (SDBusSimpleType STypeUInt32  ) = DBVUInt32  <$> getWord32
getDBV (SDBusSimpleType STypeInt64   ) = DBVInt64   <$> getInt64
getDBV (SDBusSimpleType STypeUInt64  ) = DBVUint64  <$> getWord64
getDBV (SDBusSimpleType STypeDouble  ) = DBVDouble  <$> getDouble
getDBV (SDBusSimpleType STypeUnixFD  ) = DBVUnixFD  <$> getWord32
getDBV (SDBusSimpleType STypeString  ) = DBVString  <$> getText
getDBV (SDBusSimpleType STypeObjectPath) = DBVObjectPath . objectPath <$> getText
getDBV (SDBusSimpleType STypeSignature) =  DBVSignature <$> getSignatures
getDBV STypeVariant = do
    ss <- getSignatures
    t <- case ss of
        [s] -> return s
        _ -> mzero
    case (toSing t) of
        SomeSing s-> withSingI s $ DBVVariant <$> getDBV s
getDBV (STypeArray (SDBusSimpleType STypeByte)) = do
    len <- getWord32
    DBVByteArray <$> getByteString (fromIntegral len)
getDBV (STypeArray t) = do
    len <- getWord32
    alignGet (alignment (fromSing t))
    DBVArray <$> getMany (fromIntegral len) t
getDBV (STypeStruct ts) = DBVStruct <$> getStruct ts
getDBV (STypeDict k v) = do
    len <- getWord32
    alignGet 8
    DBVDict <$> getManyPairs (fromIntegral len) (SDBusSimpleType k) v



getStruct :: Sing ts -> DBusGet (DBusStruct ts)
getStruct (SCons t SNil) = StructSingleton <$> getDBV t
getStruct (SCons t ts) = StructCons <$> getDBV t <*> getStruct ts

getMany :: Int64 -> Sing t -> DBusGet [DBusValue t]
getMany len t = do
    n <- lift B.bytesRead
    go (n+len)
  where
    go n = do
        this <- lift B.bytesRead
        case compare this n of
            LT -> do
                v <- getDBV t
                vs <- go n
                return (v:vs)
            EQ -> return []
            GT -> mzero

getManyPairs :: Int64
             -> Sing kt
             -> Sing vt
             -> DBusGet [(DBusValue kt, DBusValue vt)]
getManyPairs len kt vt = do
    n <- lift B.bytesRead
    go (n+len)
  where
    go n = do
        this <- lift B.bytesRead
        case compare this n of
            LT -> do
                k <- getDBV kt
                v <- getDBV vt
                vs <- go n
                return ((k,v):vs)
            EQ -> return []
            GT -> mzero



-- do

    -- len <- getWord32
    -- skip (alignment $ fromSing t)
    -- get
