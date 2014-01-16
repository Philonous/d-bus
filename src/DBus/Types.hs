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

module DBus.Types where

import           Control.Applicative ((<$>), (<*>))
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Trans.Error
import qualified Data.ByteString as BS
import           Data.Data(Data)
import           Data.Function (fix, on)
import           Data.Int
import           Data.List
import qualified Data.Map as Map
import           Data.Singletons.Bool
import           Data.Singletons.TH
import qualified Data.Text as Text
import           Data.Typeable(Typeable)
import           Data.Word
import           Unsafe.Coerce (unsafeCoerce)

-- import qualified DBus.Connection as DBus
-- import qualified DBus.Message as DBus


newtype ObjectPath = ObjectPath {fromObjectPath :: [Text.Text]}
                         deriving (Eq, Data, Typeable)

-- | Parse an object path. Contrary to the standard, empty path parts are ignored
objectPath = ObjectPath . filter (not . Text.null) . Text.splitOn "/"
objectPathToText (ObjectPath o) = if null o then "/"
                                            else Text.intercalate "/" o

instance Show ObjectPath where
    show = Text.unpack . objectPathToText

stripObjectPrefix :: ObjectPath -> ObjectPath -> Maybe ObjectPath
stripObjectPrefix (ObjectPath pre) (ObjectPath x) = ObjectPath <$>
                                                      stripPrefix pre x

isRoot (ObjectPath p) = null p

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
    | TypeUnit -- TODO: Remove
      -- Unit isn't actually a DBus type. It is included
      -- to make it easier to use methods without a return value
      deriving (Show, Read, Eq, Data, Typeable)

ppType :: DBusType -> String
ppType (DBusSimpleType t) = ppSimpleType t
ppType (TypeArray ts) = "[" ++ ppType ts ++ "]"
ppType (TypeStruct ts) = "(" ++ intercalate "," (ppType <$> ts) ++ ")"
ppType (TypeDict k v) = "{" ++ ppSimpleType k ++ " => " ++ ppType v ++ "}"
ppType (TypeDictEntry k v) = "<" ++ ppSimpleType k ++ " => " ++ ppType v ++ ">"
ppType TypeVariant = "Variant"
ppType TypeUnit = "()"

data Parity = Null
            | Arg Parity
              deriving (Eq, Show, Data, Typeable)

type family ArgsOf x :: Parity
type instance ArgsOf (IO x) = 'Null
type instance ArgsOf (a -> b) = 'Arg (ArgsOf b)

infixr 0 :->
data MethodDescription parity where
    (:->) :: Text.Text -> MethodDescription n -> MethodDescription (Arg n)
    Result :: Text.Text -> MethodDescription Null

genSingletons [''DBusSimpleType, ''DBusType, ''Parity]
singEqInstances [''DBusSimpleType, ''DBusType, ''Parity]
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
    -- TODO: Remove

    -- Unit isn't an actual DBus type and is included only for use with methods
    -- that don't return a value
    DBVUnit       :: DBusValue TypeUnit

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
    show (DBVUnit       ) = "DBVUnit"

typeOf :: SingI t => DBusValue t -> DBusType
typeOf (_ :: DBusValue a) = fromSing (sing :: SDBusType a)

class Representable a where
    type RepType a :: DBusType
    toRep :: a -> DBusValue (RepType a)
    fromRep :: DBusValue (RepType a) -> Maybe a

------------------------------------------------
-- Objects
------------------------------------------------
data MethodWrapper av rv where
    MReturn :: SingI t => IO (DBusValue t) -> MethodWrapper '[] t
    MAsk    :: SingI t => (DBusValue t -> MethodWrapper avs rv )
                       -> MethodWrapper (t ': avs) rv

type family ArgParity (x :: [DBusType]) :: Parity
type instance ArgParity '[] = 'Null
type instance ArgParity (x ': xs) = Arg (ArgParity xs)

data Method where
    Method :: (SingI avs, SingI t) =>
              MethodWrapper avs t
           -> Text.Text
           -> MethodDescription (ArgParity avs)
           -> Method


data Annotation = Annotation { annotationName :: Text.Text
                             , annotationValue :: Text.Text
                             } deriving (Eq, Show, Data, Typeable)

data Interface = Interface { interfaceName :: Text.Text
                           , interfaceMethods :: [Method]
                           , interfaceAnnotations :: [Annotation]
                           }

instance Eq Interface where
    (==) = (==) `on` interfaceName

data Object = Object { objectObjectPath :: ObjectPath
                     , objectInterfaces :: [Interface]
                     , objectSubObjects :: [Object]
                     }

--------------------------------------------------
-- Connection and Message
--------------------------------------------------

data MsgError = MsgError { errorName :: Text.Text
                         , errorText :: Text.Text
                         }

instance Error MsgError where
    strMsg str = MsgError { errorName = "org.freedesktop.DBus.Error.Failed"
                          , errorText = Text.pack str
                          }



data Connection = Connection { primConnection :: () -- DBus.Connection
                             , answerSlots :: TVar (Map.Map Word32
                                                    (TMVar (Either MsgError
                                                                  SomeDBusValue)))
                             , mainLoop :: ThreadId
                             }
