{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module DBus.Representable where

import           DBus.Types
import           DBus.TH

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad
import           Data.Int
import qualified Data.Map as Map
import qualified Data.Text as Text
import           Data.Word

-- instance DBusRepresentable; see DBus.Types

instance DBusRepresentable Word8 where
    type RepType Word8  = 'DBusSimpleType TypeWord8
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

instance DBusRepresentable Word32 where
    type RepType Word32 = 'DBusSimpleType TypeUnixFD
    toRep x = DBVUnixFD x
    fromRep (DBVUnixFD x) = Just x

instance DBusRepresentable Text.Text where
    type RepType Text.Text = 'DBusSimpleType TypeString
    toRep x = DBVString x
    fromRep (DBVString x) = Just x

instance DBusRepresentable ObjectPath where
    type RepType ObjectPath = 'DBusSimpleType TypeObjectPath
    toRep x = DBVObjectPath x
    fromRep (DBVObjectPath x) = Just x

instance DBusRepresentable a => DBusRepresentable [a]  where
    type RepType [a] = TypeArray (RepType a)
    toRep xs = DBVArray (map toRep xs)
    fromRep (DBVArray xs) = mapM fromRep xs

type family FromSimpleType (t :: DBusType) :: DBusSimpleType
type instance FromSimpleType (DBusSimple k) = k

instance ( Ord k
         , DBusRepresentable k
         , RepType k ~ DBusSimple r
         , DBusRepresentable v )
         => DBusRepresentable (Map.Map k v)  where
    type RepType (Map.Map k v) = TypeDict (FromSimpleType (RepType k)) (RepType v)
    toRep m = DBVDict $ map (\(l,r) -> (toRep l, toRep r)) (Map.toList m)
    fromRep (DBVDict xs) = Map.fromList <$> sequence
                           (map (\(l,r) -> (,) <$> fromRep l <*> fromRep r) xs)

$(forM [2..20] makeRepresentableTuple)
