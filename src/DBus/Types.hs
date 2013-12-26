{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DBus.Types where

import           Control.Applicative ((<$>))
import           Control.Monad
import qualified DBus as DBus
import           DBus.Client as DBs
import           Data.Function (fix)
import           Data.Int
import           Data.List (intercalate)
import           Data.Singletons.TH
import qualified Data.Text as Text
import           Data.Word

newtype ObjectPath = ObjectPath Text.Text deriving (Show, Eq)

data DBusSimpleType
    = TypeWord8
    | TypeBoolean
    | TypeInt16
    | TypeUInt16
    | TypeInt64
    | TypeUInt64
    | TypeDouble
    | TypeUnixFD
    | TypeString
    | TypeObjectPath
    | TypeSignatuer
      deriving (Show, Read, Eq)

ppSimpleType :: DBusSimpleType -> String
ppSimpleType TypeWord8      = "Word8"
ppSimpleType TypeBoolean    = "Boolean"
ppSimpleType TypeInt16      = "Int16"
ppSimpleType TypeUInt16     = "UInt16"
ppSimpleType TypeInt64      = "Int64"
ppSimpleType TypeUInt64     = "UInt64"
ppSimpleType TypeDouble     = "Double"
ppSimpleType TypeUnixFD     = "UnixFD"
ppSimpleType TypeString     = "String"
ppSimpleType TypeObjectPath = "ObjectPath"
ppSimpleType TypeSignatuer  = "Signatuer"

data DBusType
    = DBusSimpleType DBusSimpleType
    | TypeArray DBusType
    | TypeStruct [DBusType]
    | TypeDict DBusSimpleType DBusType
    | TypeVariant
      deriving (Show, Read, Eq)

ppType :: DBusType -> String
ppType (DBusSimpleType t) = ppSimpleType t
ppType (TypeArray ts) = "[" ++ ppType ts ++ "]"
ppType (TypeStruct ts) = "(" ++ intercalate "," (ppType <$> ts) ++ ")"
ppType (TypeDict k v) = "{" ++ ppSimpleType k ++ " => " ++ ppType v ++ "}"
ppType TypeVariant = "Variant"

genSingletons [''DBusSimpleType, ''DBusType]

data DBusStruct :: [DBusType] -> * where
    StructSingleton :: DBusValue a -> DBusStruct '[a]
    StructCons :: DBusValue a -> DBusStruct as -> DBusStruct (a ': as)

instance Show (DBusStruct a) where
    show xs = show $ showStruct xs

showStruct :: DBusStruct a -> [String]
showStruct (StructSingleton x) = [show x]
showStruct (StructCons x xs) = (show x : showStruct xs)

data DBusValue :: DBusType -> * where
    DBVByte       :: Word8         -> DBusValue ('DBusSimpleType TypeWord8)
    DBVBool       :: Bool          -> DBusValue ('DBusSimpleType TypeBoolean)
    DBVInt16      :: Int16         -> DBusValue ('DBusSimpleType TypeInt16)
    DBVUInt16     :: Word16        -> DBusValue ('DBusSimpleType TypeUInt16)
    DBVInt64      :: Int64         -> DBusValue ('DBusSimpleType TypeInt64)
    DBVUint64     :: Word64        -> DBusValue ('DBusSimpleType TypeUInt64)
    DBVDouble     :: Double        -> DBusValue ('DBusSimpleType TypeDouble)
    DBVUnixFD     :: Word32        -> DBusValue ('DBusSimpleType TypeUnixFD)
    DBVString     :: Text.Text     -> DBusValue ('DBusSimpleType TypeString)
    DBVObjectPath :: ObjectPath    -> DBusValue ('DBusSimpleType TypeObjectPath)
    DBVSignature  :: [DBusType]    -> DBusValue ('DBusSimpleType TypeSignatuer)
    DBVVariant    :: SingI t => DBusValue t -> DBusValue TypeVariant
    DBVArray      :: [DBusValue a] -> DBusValue (TypeArray a)
    DBVStruct     :: DBusStruct ts -> DBusValue (TypeStruct ts)

instance Show (DBusValue a) where
    show (DBVByte       x) = show x
    show (DBVBool       x) = show x
    show (DBVInt16      x) = show x
    show (DBVUInt16     x) = show x
    show (DBVInt64      x) = show x
    show (DBVUint64     x) = show x
    show (DBVDouble     x) = show x
    show (DBVUnixFD     x) = show x
    show (DBVString     x) = show x
    show (DBVObjectPath x) = show x
    show (DBVSignature  x) = show x
    show (DBVArray      x) = show x
    show (DBVStruct     x) = show x
    show (DBVVariant    (x :: DBusValue t)) = "Variant:" ++ ppType (fromSing (sing :: SDBusType t))


typeOf :: SingI t => DBusValue t -> DBusType
typeOf (_ :: DBusValue a) = fromSing (sing :: SDBusType a)

class DBusRepresentable a where
    type RepType a :: DBusType
    toRep :: a -> DBusValue (RepType a)
    fromRep :: DBusValue (RepType a) -> Maybe a
