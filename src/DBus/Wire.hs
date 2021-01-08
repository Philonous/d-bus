{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module DBus.Wire where


import           Control.Applicative          ((<$>), (<*>))
import           Control.Monad
import           Control.Monad.Catch          (MonadThrow, throwM)
import           Control.Monad.RWS
import           Control.Monad.Reader
import qualified Data.Binary.Get              as B
import qualified Data.Binary.IEEE754          as B
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy.Builder as BS
import qualified Data.Conduit                 as C
import           Data.Functor.Identity        (Identity(..))
import           Data.Int
import           Data.Singletons
import           Data.Singletons.Prelude.List
import qualified Data.Text                    as Text
import qualified Data.Text.Encoding           as Text
import           Data.Word

import           DBus.Types
import           DBus.Signature
import           DBus.Error


fromEnum' :: (Enum a, Num c) => a -> c
fromEnum' = fromIntegral . fromEnum

toEnum' :: (Enum c, Integral a) => a -> c
toEnum' = toEnum . fromIntegral

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
alignment (TypeDict _ _) = 8
alignment _ = error "alignment not defined for Unit and DictEntry"

data Endian = Little | Big
                       deriving (Show, Eq, Enum, Bounded)

type DBusPut a = RWS Endian BS.Builder Int a

endian :: (a -> BS.Builder)
       -> (a -> BS.Builder)
       -> a
       -> DBusPut ()
endian l b x = do
    e <- ask
    tell $ case e of
        Little ->  l x
        Big    ->  b x

putSize :: MonadState Int m => Int -> m ()
putSize i = modify (+i)

alignPut :: Int -> DBusPut ()
alignPut bs = do
    s <- get
    let align = ((-s) `mod` bs)
    replicateM_ align . tell $ BS.word8 0
    putSize align

sizeOf :: Int -> Int -> DBusPut a -> DBusPut Int
sizeOf offset al' x = do
    let al = if al' == 0 then 1 else al'
        offsetA = if offset == 0 then 1 else offset
    s <- ask
    here <- get
    let start = ((here `aligning` offsetA) + offset) `aligning` al
    return $ (fst (execRWS x s start)) - start
  where
    aligning x' al = x' + ((-x') `mod` al)

bytes :: (a -> BS.Builder)
      -> (a -> BS.Builder)
      -> Int
      -> a
      -> DBusPut ()
bytes l b bs x = alignPut bs >> endian l b x >> putSize bs

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

putSignatures :: [DBusType] -> RWST Endian BS.Builder Int Identity ()
putSignatures s = do
    let bs = toSignatures s
        len = BS.length bs
    when (len > 255) $ error "Signature too long"
    putWord8 $ fromIntegral len
    putByteString bs
    putWord8 0

putDBV :: SingI t => DBusValue t -> DBusPut ()
putDBV = putDBV' sing

putDBV' :: Sing t -> DBusValue t -> DBusPut ()
putDBV' _ (DBVByte       x) = putWord8 x
putDBV' _ (DBVBool       x) = putWord32 $ fromEnum' x
putDBV' _ (DBVInt16      x) = putInt16 x
putDBV' _ (DBVUInt16     x) = putWord16 x
putDBV' _ (DBVInt32      x) = putInt32 x
putDBV' _ (DBVUInt32     x) = putWord32 x
putDBV' _ (DBVInt64      x) = putInt64 x
putDBV' _ (DBVUInt64     x) = putWord64 x
putDBV' _ (DBVDouble     x) = putDouble x
putDBV' _ (DBVUnixFD     x) = putWord32 x
putDBV' _ (DBVString     x) = putText x
putDBV' _ (DBVObjectPath x) = putObjectPath x
putDBV' _ (DBVSignature  x) = putSignatures x
putDBV' _ (DBVVariant    x) = do
    putSignatures [typeOf x]
    putDBV' sing x
putDBV' (STypeArray t) (DBVArray x) = do
    let content = mapM_ (putDBV' t) x
    let al = alignment $ fromSing t
    size <- sizeOf 4 al content
    putWord32 $ fromIntegral size
    alignPut al
    content
putDBV' _ (DBVByteArray  x) = do
    putWord32 . fromIntegral $ BS.length x
    putByteString x
putDBV' (STypeStruct ts) (DBVStruct     x) = do
    alignPut 8
    putStruct ts x
putDBV' (STypeDict kt vt) (DBVDict       x) =
    let content = forM_ x $ \(k,v) -> do
            alignPut 8
            putDBV' (SDBusSimpleType kt) k
            putDBV' vt v
    in do
        putWord32 . fromIntegral =<< sizeOf 4 8 content
        alignPut 8
        content
putDBV' _ DBVUnit = error "putDBV' not defined for Unit"

putStruct :: Sing a -> DBusStruct a -> DBusPut ()
putStruct (SCons t SNil) (StructSingleton v) = putDBV' t v
putStruct (SCons t ts) (StructCons v vs ) = putDBV' t v >> putStruct ts vs
putStruct _ _ = error "putStruct: impossible case"

runDBusPut :: Num s => r -> RWS r b s a -> b
runDBusPut e x = snd $ evalRWS x e 0

putValues :: [SomeDBusValue]
          -> DBusPut ()
putValues vs = mapM_ (\(DBV v) -> putDBV v) vs

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
alignGet bs = do
    s <- fromIntegral <$> lift B.bytesRead
    let align = ((-s) `mod` bs)
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
getInt16 = getting B.getInt16le B.getInt16be 2

getInt32 :: DBusGet Int32
getInt32 = getting B.getInt32le B.getInt32be 4

getInt64 :: DBusGet Int64
getInt64 = getting B.getInt64le B.getInt64be 8

getDouble :: DBusGet Double
getDouble = getting B.getFloat64le B.getFloat64be 8

getText :: DBusGet Text.Text
getText = do
    len <- getWord32
    bs <- lift $ B.getByteString (fromIntegral len)
    guard . (== 0) =<< getWord8
    case Text.decodeUtf8' bs of
        Left _ -> fail "could not decode UTF 8"
        Right txt -> return txt

getBool :: DBusGet Bool
getBool = toEnum' <$> getWord32

getSignatures :: ReaderT Endian B.Get [DBusType]
getSignatures = do
    len <- getWord8
    bs <- lift $ B.getByteString (fromIntegral len)
    guard . (0 ==) =<< getWord8
    case parseSigs bs of
         Nothing -> fail $ "could not parse signature" ++ show bs
         Just s -> return s

getByteString :: MonadTrans t => Int -> t B.Get BS.ByteString
getByteString = lift . B.getByteString

getDBV :: SingI t => DBusGet (DBusValue t)
getDBV = getDBV' sing

getDBVByType :: DBusType -> DBusGet SomeDBusValue
getDBVByType t = case toSing t of
    SomeSing s -> withSingI s $ DBV <$> getDBV' s

getDBV' :: Sing t -> DBusGet (DBusValue t)
getDBV' (SDBusSimpleType STypeByte    ) = DBVByte    <$> getWord8
getDBV' (SDBusSimpleType STypeBoolean ) = DBVBool    <$> getBool
getDBV' (SDBusSimpleType STypeInt16   ) = DBVInt16   <$> getInt16
getDBV' (SDBusSimpleType STypeUInt16  ) = DBVUInt16  <$> getWord16
getDBV' (SDBusSimpleType STypeInt32   ) = DBVInt32   <$> getInt32
getDBV' (SDBusSimpleType STypeUInt32  ) = DBVUInt32  <$> getWord32
getDBV' (SDBusSimpleType STypeInt64   ) = DBVInt64   <$> getInt64
getDBV' (SDBusSimpleType STypeUInt64  ) = DBVUInt64  <$> getWord64
getDBV' (SDBusSimpleType STypeDouble  ) = DBVDouble  <$> getDouble
getDBV' (SDBusSimpleType STypeUnixFD  ) = DBVUnixFD  <$> getWord32
getDBV' (SDBusSimpleType STypeString  ) = DBVString  <$> getText
getDBV' (SDBusSimpleType STypeObjectPath) = DBVObjectPath . objectPath <$> getText
getDBV' (SDBusSimpleType STypeSignature) =  DBVSignature <$> getSignatures
getDBV' STypeVariant = do
    ss <- getSignatures
    t <- case ss of
        [s] -> return s
        s  -> fail $ "Expected 1 signature, got " ++ show (length s)
    case (toSing t) of
        SomeSing s -> withSingI s $ DBVVariant <$> getDBV' s
getDBV' (STypeArray (SDBusSimpleType STypeByte)) = do
    len <- getWord32
    DBVByteArray <$> getByteString (fromIntegral len)
getDBV' (STypeArray t) = do
    len <- getWord32
    alignGet (alignment (fromSing t))
    DBVArray <$> getMany (fromIntegral len) t
getDBV' (STypeStruct ts) = do
    alignGet 8
    DBVStruct <$> getStruct ts
getDBV' (STypeDict k v) = do
    len <- getWord32
    alignGet 8
    DBVDict <$> getManyPairs (fromIntegral len) (SDBusSimpleType k) v
getDBV' _ = error "getDBV not defined for DictEntry and Unit"

getStruct :: Sing ts -> DBusGet (DBusStruct ts)
getStruct (SCons t SNil) = StructSingleton <$> getDBV' t
getStruct (SCons t ts) = StructCons <$> getDBV' t <*> getStruct ts
getStruct SNil = error "getStruct: Empty struct is impossible"

getMany :: Int64 -> Sing t -> DBusGet [DBusValue t]
getMany len t = do
    n <- lift B.bytesRead
    go (n + len)
  where
    go n = do
        this <- lift B.bytesRead
        case compare this n of
            LT -> do
                v <- getDBV' t
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
                alignGet 8
                k <- getDBV' kt
                v <- getDBV' vt
                vs <- go n
                return ((k,v):vs)
            EQ -> return []
            GT -> mzero

-- | Run a Binary Getter as a Sink
sinkGet :: MonadThrow m => B.Get b -> C.ConduitM BS.ByteString o m b
sinkGet f = sink (B.runGetIncremental f)
  where
      sink (B.Done bs _ v)  = C.leftover bs >> return v
      sink (B.Fail u o e)   = throwM $ DBusParseError $ e
                                ++ " (" ++ show o ++ "; "
                                ++ show u ++ ")"
      sink (B.Partial next) = do
        bs <- C.await
        sink $ next bs
