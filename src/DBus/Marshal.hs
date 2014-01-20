{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module DBus.Marshal where

import           Control.Applicative ((<$>), liftA2)
import           Data.Maybe
import           Data.Singletons
import           Data.Singletons.List (Sing(..))
import qualified Data.Text.Encoding as Text

import qualified DBus.Message as DBus

import           DBus.Types
import           DBus.Signature

-- toTypeS = toType . fromSing

toArg :: SingI t => DBusValue t -> DBus.Arg
toArg x = toArg' sing x

toArg' :: SDBusType t -> DBusValue t -> DBus.Arg
toArg' _ (DBVByte       x) = DBus.Byte x
toArg' _ (DBVBool       x) = DBus.Boolean x
toArg' _ (DBVInt16      x) = DBus.Int16 x
toArg' _ (DBVUInt16     x) = DBus.Word16 x
toArg' _ (DBVInt32      x) = DBus.Int32 x
toArg' _ (DBVUInt32     x) = DBus.Word32 x
toArg' _ (DBVInt64      x) = DBus.Int64 x
toArg' _ (DBVUint64     x) = DBus.Word64 x
toArg' _ (DBVDouble     x) = DBus.Double x
toArg' _ (DBVString     x) = DBus.String (Text.encodeUtf8 x)
toArg' _ (DBVObjectPath x) = DBus.ObjectPath . Text.encodeUtf8 $ objectPathToText x
toArg' _ (DBVSignature  x) = DBus.TypeSignature $ toSignatures x
toArg' _ (DBVUnixFD     x) = error "toValue DBVUnixFD is not implemented"
toArg' _ (DBVVariant    x) = DBus.Variant $ toArg' sing x
toArg' (STypeArray t)   (DBVArray    x) = DBus.Array (toSignature $ fromSing t)
                                             (map (toArg' t) x)
toArg' _ (DBVByteArray  x) = DBus.ByteString x
toArg'  ts (DBVStruct   x) = DBus.Struct $ structToArgs ts x
toArg' t@(STypeDict kt vt) (DBVDict     x) =
    DBus.Array (toSignature $ fromSing t)
    .  map (\(k,v) -> DBus.DictEntry (toArg' (SDBusSimpleType kt) k)
                                     (toArg' vt v) ) $ x

toArg' _ DBVUnit = error "Can't marshal DBVUnit"

structToArgs :: SDBusType (TypeStruct ts) -> DBusStruct ts -> [DBus.Arg]
structToArgs (STypeStruct (SCons t SNil)) (StructSingleton x) = [toArg' t x]
structToArgs (STypeStruct (SCons t ts)) (StructCons x xs)
    = toArg' t x : structToArgs (STypeStruct ts) xs

fromArg :: DBus.Arg -> SomeDBusValue
fromArg (DBus.Boolean x) = DBV $ DBVBool  x
fromArg (DBus.Byte   x) = DBV $ DBVByte   x
fromArg (DBus.Int16  x) = DBV $ DBVInt16  x
fromArg (DBus.Word16 x) = DBV $ DBVUInt16 x
fromArg (DBus.Int32  x) = DBV $ DBVInt32  x
fromArg (DBus.Word32 x) = DBV $ DBVUInt32 x
fromArg (DBus.Int64  x) = DBV $ DBVInt64  x
fromArg (DBus.Word64 x) = DBV $ DBVUint64 x
fromArg (DBus.Double x) = DBV $ DBVDouble x
fromArg (DBus.String x) = DBV $ DBVString (Text.decodeUtf8 x)
fromArg (DBus.ObjectPath x) = DBV . DBVObjectPath . objectPath $ Text.decodeUtf8 x
fromArg (DBus.TypeSignature ts) = DBV $ DBVSignature (fromJust $ parseSigs ts)
fromArg (DBus.Variant v) = DBV ((\(DBV v) -> DBVVariant v) $ fromArg v)
fromArg (DBus.Array t v) = case (toSing . fromJust $ parseSig t) of
    SomeSing sb -> case sb of
        (STypeDictEntry ktS vtS) -> case (ktS, vtS) of
            (_ :: Sing ktt, _ :: Sing vtt) ->
                withSingI ktS $ withSingI vtS $
                    let (d :: DBusValue (TypeDict ktt vtt))
                            = DBVDict . catMaybes
                              . map (\(DBus.DictEntry k v) -> liftA2 (,)
                                                          (dbusValue $ fromArg k)
                                                          (dbusValue $ fromArg v))
                                $ v
                    in DBV d
        (sb :: Sing t) -> withSingI sb
                   (let (a :: DBusValue (TypeArray t))
                            = DBVArray $ catMaybes (dbusValue . fromArg <$> v)
                    in DBV a)
fromArg (DBus.ByteString bs) = DBV $ DBVByteArray bs
fromArg (DBus.Struct xs) = case argsToStruct xs of
    (SDBS s) -> DBV (DBVStruct s)


argsToStruct :: [DBus.Arg] -> SomeDBusStruct
argsToStruct [] = error "argsToStruct: empty list"
argsToStruct [x] = case fromArg x of
                         (DBV s) -> SDBS (StructSingleton s)
argsToStruct (x:xs) = case fromArg x of
                          (DBV s) -> case argsToStruct xs of
                              (SDBS ss) -> SDBS (StructCons s ss)
