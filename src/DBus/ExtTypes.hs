{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module DBus.ExtTypes where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad
import qualified Data.ByteString as BS
import           Data.Data(Data)
import           Data.Function (fix)
import           Data.Int
import           Data.List (intercalate)
import qualified Data.Map as Map
import           Data.Singletons.Bool
import           Data.Singletons.TH
import qualified Data.Text as Text
import           Data.Typeable(Typeable)
import           Data.Word
import           Unsafe.Coerce (unsafeCoerce)

newtype ObjectPath = ObjectPath Text.Text deriving (Show, Eq)

data DBusSimpleType
    = TypeByte
    | TypeBoolean
    | TypeInt16
    | TypeUInt16
    | TypeInt32
    | TypeUInt32
    | TypeInt64
    | TypeUInt64
    | TypeDouble
    | TypeUnixFD
    | TypeString
    | TypeObjectPath
    | TypeSignature
      deriving (Show, Read, Eq, Data, Typeable)

ppSimpleType :: DBusSimpleType -> String
ppSimpleType TypeByte       = "Word8"
ppSimpleType TypeBoolean    = "Boolean"
ppSimpleType TypeInt16      = "Int16"
ppSimpleType TypeUInt16     = "UInt16"
ppSimpleType TypeInt32      = "Int32"
ppSimpleType TypeUInt32     = "UInt32"
ppSimpleType TypeInt64      = "Int64"
ppSimpleType TypeUInt64     = "UInt64"
ppSimpleType TypeDouble     = "Double"
ppSimpleType TypeUnixFD     = "UnixFD"
ppSimpleType TypeString     = "String"
ppSimpleType TypeObjectPath = "ObjectPath"
ppSimpleType TypeSignature  = "Signature"

data DBusType
    = DBusSimpleType DBusSimpleType
    | TypeArray DBusType
    | TypeStruct [DBusType]
    | TypeDict DBusSimpleType DBusType
    | TypeVariant
    | TypeDictEntry DBusSimpleType DBusType
      deriving (Show, Read, Eq, Data, Typeable)

ppType :: DBusType -> String
ppType (DBusSimpleType t) = ppSimpleType t
ppType (TypeArray ts) = "[" ++ ppType ts ++ "]"
ppType (TypeStruct ts) = "(" ++ intercalate "," (ppType <$> ts) ++ ")"
ppType (TypeDict k v) = "{" ++ ppSimpleType k ++ " => " ++ ppType v ++ "}"
ppType (TypeDictEntry k v) = "<" ++ ppSimpleType k ++ " => " ++ ppType v ++ ">"
ppType TypeVariant = "Variant"

genSingletons [''DBusSimpleType, ''DBusType]
singEqInstances [''DBusSimpleType, ''DBusType]
-- singDecideInstances [''DBusSimpleType]

data DBusStruct :: [DBusType] -> * where
    StructSingleton :: DBusValue a -> DBusStruct '[a]
    StructCons :: DBusValue a -> DBusStruct as -> DBusStruct (a ': as)

data SomeDBusStruct where
    SDBS :: SingI ts => DBusStruct ts -> SomeDBusStruct

instance Show (DBusStruct a) where
    show xs = show $ showStruct xs

showStruct :: DBusStruct a -> [String]
showStruct (StructSingleton x) = [show x]
showStruct (StructCons x xs) = (show x : showStruct xs)

data DBusValue :: DBusType -> * where
    DBVByte       :: Word8         -> DBusValue ('DBusSimpleType TypeByte)
    DBVBool       :: Bool          -> DBusValue ('DBusSimpleType TypeBoolean)
    DBVInt16      :: Int16         -> DBusValue ('DBusSimpleType TypeInt16)
    DBVUInt16     :: Word16        -> DBusValue ('DBusSimpleType TypeUInt16)
    DBVInt32      :: Int32         -> DBusValue ('DBusSimpleType TypeInt32)
    DBVUInt32     :: Word32        -> DBusValue ('DBusSimpleType TypeUInt32)
    DBVInt64      :: Int64         -> DBusValue ('DBusSimpleType TypeInt64)
    DBVUint64     :: Word64        -> DBusValue ('DBusSimpleType TypeUInt64)
    DBVDouble     :: Double        -> DBusValue ('DBusSimpleType TypeDouble)
    DBVUnixFD     :: Word32        -> DBusValue ('DBusSimpleType TypeUnixFD)
    DBVString     :: Text.Text     -> DBusValue ('DBusSimpleType TypeString)
    DBVObjectPath :: ObjectPath    -> DBusValue ('DBusSimpleType TypeObjectPath)
    DBVSignature  :: [DBusType]    -> DBusValue ('DBusSimpleType TypeSignature)
    DBVVariant    :: (SingI t )    => DBusValue t -> DBusValue TypeVariant
    DBVArray      :: [DBusValue a] -> DBusValue (TypeArray a)
    DBVByteArray  :: BS.ByteString -> DBusValue (TypeArray ('DBusSimpleType TypeByte))
    DBVStruct     :: DBusStruct ts -> DBusValue (TypeStruct ts)
    DBVDict       :: [(DBusValue ('DBusSimpleType k) ,DBusValue v)]
                                   -> DBusValue (TypeDict k v)

-- TODO: Reinstate once https://github.com/goldfirere/singletons/issues/2 is
-- resolved

-- fromVariant :: SingI t => DBusValue TypeVariant -> Maybe (DBusValue t)
-- fromVariant (DBVVariant (v :: DBusValue s))
--     = fix $ \(_ :: Maybe (DBusValue t)) ->
--         let ss = (sing :: Sing s)
--             st = (sing :: Sing t)
--         in case (ss %~ st) of
--             Proved Refl -- Bring into scope a proof that s~t
--                 -> Just v
--             Disproved _ -> Nothing

castDBV :: (SingI s, SingI t) => DBusValue s -> Maybe (DBusValue t)
castDBV (v :: DBusValue s)
    = fix $ \(_ :: Maybe (DBusValue t)) ->
        let ss = (sing :: Sing s)
            st = (sing :: Sing t)
        in case (ss %:== st) of
            STrue -> Just (unsafeCoerce v)
            SFalse -> Nothing

data SomeDBusValue where
    DBV :: SingI t => DBusValue t -> SomeDBusValue

dbusValue :: SingI t => SomeDBusValue -> Maybe (DBusValue t)
dbusValue (DBV v) = castDBV v

dbusSValue :: SingI t => SomeDBusValue -> Maybe (DBusValue ('DBusSimpleType t))
dbusSValue (DBV v) = castDBV v

-- | Extract a DBusValue from a Variant iff the type matches or return nothing
fromVariant :: SingI t => DBusValue TypeVariant -> Maybe (DBusValue t)
fromVariant (DBVVariant v) = castDBV v

instance Show (DBusValue a) where
    show (DBVByte       x) = show x
    show (DBVBool       x) = show x
    show (DBVInt16      x) = show x
    show (DBVUInt16     x) = show x
    show (DBVInt32      x) = show x
    show (DBVUInt32     x) = show x
    show (DBVInt64      x) = show x
    show (DBVUint64     x) = show x
    show (DBVDouble     x) = show x
    show (DBVUnixFD     x) = show x
    show (DBVString     x) = show x
    show (DBVObjectPath x) = show x
    show (DBVSignature  x) = show x
    show (DBVArray      x) = show x
    show (DBVByteArray  x) = show x
    show (DBVStruct     x) = show x
    show (DBVVariant    (x :: DBusValue t)) = "Variant:" ++ ppType (fromSing (sing :: SDBusType t)) ++ "=" ++ show x
    show (DBVDict      x) = show x


typeOf :: SingI t => DBusValue t -> DBusType
typeOf (_ :: DBusValue a) = fromSing (sing :: SDBusType a)

class DBusRepresentable a where
    type RepType a :: DBusType
    toRep :: a -> DBusValue (RepType a)
    fromRep :: DBusValue (RepType a) -> Maybe a

instance DBusRepresentable Word8 where
    type RepType Word8  = 'DBusSimpleType TypeByte
    toRep x = DBVByte x
    fromRep (DBVByte x) = Just x

instance DBusRepresentable Bool where
    type RepType Bool = 'DBusSimpleType TypeBoolean
    toRep x = DBVBool x
    fromRep (DBVBool x) = Just x

instance DBusRepresentable Int16 where
    type RepType Int16 = 'DBusSimpleType TypeInt16
    toRep x = DBVInt16 x
    fromRep (DBVInt16 x) = Just x

instance DBusRepresentable Word16 where
    type RepType Word16 = 'DBusSimpleType TypeUInt16
    toRep x = DBVUInt16 x
    fromRep (DBVUInt16 x) = Just x

instance DBusRepresentable Int32 where
    type RepType Int32 = 'DBusSimpleType TypeInt32
    toRep x = DBVInt32 x
    fromRep (DBVInt32 x) = Just x

instance DBusRepresentable Word32 where
    type RepType Word32 = 'DBusSimpleType TypeUInt32
    toRep x = DBVUInt32 x
    fromRep (DBVUInt32 x) = Just x

instance DBusRepresentable Int64 where
    type RepType Int64 = 'DBusSimpleType TypeInt64
    toRep x = DBVInt64 x
    fromRep (DBVInt64 x) = Just x

instance DBusRepresentable Word64 where
    type RepType Word64 = 'DBusSimpleType TypeUInt64
    toRep x = DBVUint64 x
    fromRep (DBVUint64 x) = Just x

instance DBusRepresentable Double where
    type RepType Double = 'DBusSimpleType TypeDouble
    toRep x = DBVDouble x
    fromRep (DBVDouble x) = Just x

instance DBusRepresentable Text.Text where
    type RepType Text.Text = 'DBusSimpleType TypeString
    toRep x = DBVString x
    fromRep (DBVString x) = Just x

instance DBusRepresentable ObjectPath where
    type RepType ObjectPath = 'DBusSimpleType TypeObjectPath
    toRep x = DBVObjectPath x
    fromRep (DBVObjectPath x) = Just x

instance ( DBusRepresentable a , SingI (RepType a))
         => DBusRepresentable [a]  where
    type RepType [a] = TypeArray (RepType a)
    toRep xs = DBVArray $ map toRep xs
    fromRep (DBVArray xs) = mapM fromRep xs
    fromRep (DBVByteArray bs) = fromRep . DBVArray . map DBVByte $ BS.unpack bs

instance DBusRepresentable a => DBusRepresentable BS.ByteString  where
    type RepType BS.ByteString = TypeArray ('DBusSimpleType  TypeByte)
    toRep bs = DBVByteArray bs
    fromRep (DBVByteArray bs) = Just bs
    fromRep (DBVArray bs) = BS.pack <$> mapM fromRep bs

type family FromSimpleType (t :: DBusType) :: DBusSimpleType
type instance FromSimpleType ('DBusSimpleType k) = k

instance ( Ord k
         , DBusRepresentable k
         , RepType k ~ 'DBusSimpleType r
         , DBusRepresentable v )
         => DBusRepresentable (Map.Map k v)  where
    type RepType (Map.Map k v) = TypeDict (FromSimpleType (RepType k)) (RepType v)
    toRep m = DBVDict $ map (\(l,r) -> (toRep l, toRep r)) (Map.toList m)
    fromRep (DBVDict xs) = Map.fromList <$> sequence
                           (map (\(l,r) -> (,) <$> fromRep l <*> fromRep r) xs)

instance ( DBusRepresentable l
         , DBusRepresentable r
         , SingI (RepType l)
         , SingI (RepType r))
         => DBusRepresentable (Either l r) where
    type RepType (Either l r) = TypeStruct '[ 'DBusSimpleType TypeBoolean
                                            , TypeVariant]
    toRep (Left l) = DBVStruct ( StructCons (DBVBool False) $
                                 StructSingleton (DBVVariant (toRep l)))
    toRep (Right r) = DBVStruct ( StructCons (DBVBool True) $
                                 StructSingleton (DBVVariant (toRep r)))
    fromRep (DBVStruct ((StructCons (DBVBool False)
              (StructSingleton r))))
             = Left <$> (fromRep =<< fromVariant r)
    fromRep (DBVStruct ((StructCons (DBVBool True)
              (StructSingleton r))))
             = Right <$> (fromRep =<< fromVariant r)
