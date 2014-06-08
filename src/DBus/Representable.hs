{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fcontext-stack=21 #-}

module DBus.Representable where

import           DBus.Types
import           DBus.TH

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad
import           Data.Int
import qualified Data.Map as Map
import           Data.Singletons
import qualified Data.Text as Text
import           Data.Word
import qualified Data.ByteString as BS

-- class Representable; see DBus.Types
forM [2..20] makeRepresentableTuple

instance Representable () where
    type RepType () = TypeUnit
    toRep _ = DBVUnit
    fromRep DBVUnit = Just ()

instance Representable Word8 where
    type RepType Word8  = 'DBusSimpleType TypeByte
    toRep x = DBVByte x
    fromRep (DBVByte x) = Just x

instance Representable Bool where
    type RepType Bool = 'DBusSimpleType TypeBoolean
    toRep x = DBVBool x
    fromRep (DBVBool x) = Just x

instance Representable Int16 where
    type RepType Int16 = 'DBusSimpleType TypeInt16
    toRep x = DBVInt16 x
    fromRep (DBVInt16 x) = Just x

instance Representable Word16 where
    type RepType Word16 = 'DBusSimpleType TypeUInt16
    toRep x = DBVUInt16 x
    fromRep (DBVUInt16 x) = Just x

instance Representable Int32 where
    type RepType Int32 = 'DBusSimpleType TypeInt32
    toRep x = DBVInt32 x
    fromRep (DBVInt32 x) = Just x

instance Representable Word32 where
    type RepType Word32 = 'DBusSimpleType TypeUInt32
    toRep x = DBVUInt32 x
    fromRep (DBVUInt32 x) = Just x

instance Representable Int64 where
    type RepType Int64 = 'DBusSimpleType TypeInt64
    toRep x = DBVInt64 x
    fromRep (DBVInt64 x) = Just x

instance Representable Word64 where
    type RepType Word64 = 'DBusSimpleType TypeUInt64
    toRep x = DBVUInt64 x
    fromRep (DBVUInt64 x) = Just x

instance Representable Double where
    type RepType Double = 'DBusSimpleType TypeDouble
    toRep x = DBVDouble x
    fromRep (DBVDouble x) = Just x

instance Representable Text.Text where
    type RepType Text.Text = 'DBusSimpleType TypeString
    toRep x = DBVString x
    fromRep (DBVString x) = Just x

instance Representable ObjectPath where
    type RepType ObjectPath = 'DBusSimpleType TypeObjectPath
    toRep x = DBVObjectPath x
    fromRep (DBVObjectPath x) = Just x

instance SingI t => Representable (DBusValue t) where
    type RepType (DBusValue t) = t
    toRep = id
    fromRep = Just

instance ( Representable a , SingI (RepType a))
         => Representable [a]  where
    type RepType [a] = TypeArray (RepType a)
    toRep xs = DBVArray $ map toRep xs
    fromRep (DBVArray xs) = mapM fromRep xs
    fromRep (DBVByteArray bs) = fromRep . DBVArray . map DBVByte $ BS.unpack bs

instance Representable BS.ByteString  where
    type RepType BS.ByteString = TypeArray ('DBusSimpleType  TypeByte)
    toRep bs = DBVByteArray bs
    fromRep (DBVByteArray bs) = Just bs
    fromRep (DBVArray bs) = BS.pack <$> mapM fromRep bs

type family FromSimpleType (t :: DBusType) :: DBusSimpleType where
    FromSimpleType ('DBusSimpleType k) = k

instance ( Ord k
         , Representable k
         , RepType k ~ 'DBusSimpleType r
         , SingI r
         , Representable v )
         => Representable (Map.Map k v)  where
    type RepType (Map.Map k v) = TypeDict (FromSimpleType (RepType k)) (RepType v)
    toRep m = DBVDict $ map (\(l,r) -> (toRep l, toRep r)) (Map.toList m)
    fromRep (DBVDict xs) = Map.fromList <$> sequence
                           (map (\(l,r) -> (,) <$> fromRep l <*> fromRep r) xs)
