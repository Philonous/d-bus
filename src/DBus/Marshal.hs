{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module DBus.Marshal where

import           Control.Applicative ((<$>), (<*>), liftA2)
import           Control.Arrow ((***))
import qualified DBus as DBus
import           DBus.ExtTypes
import           DBus.Representable
import qualified DBus.Types as DBus
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Singletons
import           Data.Singletons.List (Sing(..))
import qualified Data.Text as Text
import           Data.Vector (fromList, toList)
import           GHC.Prim
import           Unsafe.Coerce (unsafeCoerce)

-- toStructure :: DBusStruct t -> [DBus.Variant]
-- toStructure (StructSingleton x) = [toVariant x]
-- toStructure (StructCons x xs) = toVariant x : toStructure xs

toType :: DBusType -> DBus.Type
toType (DBusSimpleType TypeByte)       = DBus.TypeWord8
toType (DBusSimpleType TypeBoolean)    = DBus.TypeBoolean
toType (DBusSimpleType TypeInt16)      = DBus.TypeInt16
toType (DBusSimpleType TypeUInt16)     = DBus.TypeWord16
toType (DBusSimpleType TypeInt32)      = DBus.TypeInt32
toType (DBusSimpleType TypeUInt32)     = DBus.TypeWord32
toType (DBusSimpleType TypeInt64)      = DBus.TypeInt64
toType (DBusSimpleType TypeUInt64)     = DBus.TypeWord64
toType (DBusSimpleType TypeDouble)     = DBus.TypeDouble
toType (DBusSimpleType TypeUnixFD)     = error "toType TypeUnixFD: not implemented"
toType (DBusSimpleType TypeString)     = DBus.TypeString
toType (DBusSimpleType TypeObjectPath) = DBus.TypeObjectPath
toType (DBusSimpleType TypeSignature)  = DBus.TypeSignature
toType (TypeArray t)                   = DBus.TypeArray $ toType t
toType (TypeStruct ts )                = DBus.TypeStructure $ map toType ts
toType (TypeDict kt vt)                = DBus.TypeDictionary
                                             (toType $ DBusSimpleType kt)
                                             (toType vt)
toType TypeVariant                     = DBus.TypeVariant

fromType :: DBus.Type -> DBusType
fromType DBus.TypeWord8              = (DBusSimpleType TypeByte)
fromType DBus.TypeBoolean            = (DBusSimpleType TypeBoolean)
fromType DBus.TypeInt16              = (DBusSimpleType TypeInt16)
fromType DBus.TypeWord16             = (DBusSimpleType TypeUInt16)
fromType DBus.TypeInt32              = (DBusSimpleType TypeInt32)
fromType DBus.TypeWord32             = (DBusSimpleType TypeUInt32)
fromType DBus.TypeInt64              = (DBusSimpleType TypeInt64)
fromType DBus.TypeWord64             = (DBusSimpleType TypeUInt64)
fromType DBus.TypeDouble             = (DBusSimpleType TypeDouble)
fromType DBus.TypeString             = (DBusSimpleType TypeString)
fromType DBus.TypeObjectPath         = (DBusSimpleType TypeObjectPath)
fromType DBus.TypeSignature          = (DBusSimpleType TypeSignature)
fromType (DBus.TypeArray t)          = (TypeArray $ fromType t)
fromType (DBus.TypeStructure ts)     = (TypeStruct $ map fromType ts )
fromType DBus.TypeVariant            = TypeVariant
fromType (DBus.TypeDictionary kt vt) = TypeDict (unSimple $ fromType kt)
                                                (fromType vt)
  where
    unSimple (DBusSimpleType t) = t
    unSimple e = error $ "FromType: Dictionary had complex key type " ++ show e

toAtom :: DBusValue ('DBusSimpleType t) -> DBus.Atom
toAtom (DBVByte       x)              = DBus.AtomWord8 x
toAtom (DBVBool       x)              = DBus.AtomBool x
toAtom (DBVInt16      x)              = DBus.AtomInt16 x
toAtom (DBVUInt16     x)              = DBus.AtomWord16 x
toAtom (DBVInt32      x)              = DBus.AtomInt32 x
toAtom (DBVUInt32     x)              = DBus.AtomWord32 x
toAtom (DBVInt64      x)              = DBus.AtomInt64 x
toAtom (DBVUint64     x)              = DBus.AtomWord64 x
toAtom (DBVDouble     x)              = DBus.AtomDouble x
toAtom (DBVString     x)              = DBus.AtomText x
toAtom (DBVObjectPath (ObjectPath x)) = DBus.AtomObjectPath
                                            (DBus.ObjectPath $ Text.unpack x)
toAtom (DBVSignature  x)              = DBus.AtomSignature
                                            (DBus.Signature $ map toType x)
toAtom (DBVUnixFD     x)              = error "toValue DBVUnixFD is not implemented"

toTypeS = toType . fromSing

toValue :: SingI t => DBusValue t -> DBus.Value
toValue x = toValue' sing x

toValue' :: SDBusType t -> DBusValue t -> DBus.Value
toValue' _ v@DBVByte{}       = DBus.ValueAtom $ toAtom v
toValue' _ v@DBVBool{}       = DBus.ValueAtom $ toAtom v
toValue' _ v@DBVInt16{}      = DBus.ValueAtom $ toAtom v
toValue' _ v@DBVUInt16{}     = DBus.ValueAtom $ toAtom v
toValue' _ v@DBVInt32{}      = DBus.ValueAtom $ toAtom v
toValue' _ v@DBVUInt32{}     = DBus.ValueAtom $ toAtom v
toValue' _ v@DBVInt64{}      = DBus.ValueAtom $ toAtom v
toValue' _ v@DBVUint64{}     = DBus.ValueAtom $ toAtom v
toValue' _ v@DBVDouble{}     = DBus.ValueAtom $ toAtom v
toValue' _ v@DBVString{}     = DBus.ValueAtom $ toAtom v
toValue' _ v@DBVObjectPath{} = DBus.ValueAtom $ toAtom v
toValue' _ v@DBVSignature{}  = DBus.ValueAtom $ toAtom v
toValue' _ v@DBVUnixFD{}     = DBus.ValueAtom $ toAtom v
toValue' _ (DBVVariant x)    = DBus.ValueVariant . DBus.Variant
                                   $ toValue' sing x
toValue' (STypeArray t) a@(DBVArray    x) = DBus.ValueVector (toTypeS t)
                                             (fromList $ map (toValue' t) x)
toValue' _ (DBVByteArray  x) = DBus.ValueBytes x
toValue'  ts (DBVStruct   x) = DBus.ValueStructure $ structToValues ts x
toValue' t (DBVDict     x) = case t of
    STypeDict kt vt -> DBus.ValueMap (toType (DBusSimpleType (fromSing kt)))
                                     (toType $ fromSing vt)
                          . Map.fromList . map (toAtom *** toValue' vt) $ x

structToValues :: SDBusType (TypeStruct ts) -> DBusStruct ts -> [DBus.Value]
structToValues (STypeStruct (SCons t SNil)) (StructSingleton x) = [toValue' t x]
structToValues (STypeStruct (SCons t ts)) (StructCons x xs)
    = toValue' t x : structToValues (STypeStruct ts) xs


fromAtom :: DBus.Atom -> SomeDBusValue
fromAtom (DBus.AtomBool   x) = DBV $ DBVBool   x
fromAtom (DBus.AtomInt16  x) = DBV $ DBVInt16  x
fromAtom (DBus.AtomWord16 x) = DBV $ DBVUInt16 x
fromAtom (DBus.AtomInt32  x) = DBV $ DBVInt32  x
fromAtom (DBus.AtomWord32 x) = DBV $ DBVUInt32 x
fromAtom (DBus.AtomInt64  x) = DBV $ DBVInt64  x
fromAtom (DBus.AtomWord64 x) = DBV $ DBVUint64 x
fromAtom (DBus.AtomDouble x) = DBV $ DBVDouble x
fromAtom (DBus.AtomText   x) = DBV $ DBVString x
fromAtom (DBus.AtomObjectPath (DBus.ObjectPath x))
    = DBV $ DBVObjectPath (ObjectPath $ Text.pack x)
fromAtom (DBus.AtomSignature (DBus.Signature ts))
    = DBV $ DBVSignature (map fromType ts)

fromValue (DBus.ValueAtom a) = fromAtom a
fromValue (DBus.ValueVariant (DBus.Variant v))
    = DBV ((\(DBV v) -> DBVVariant v) $ fromValue v)
fromValue (DBus.ValueVector t v) = case (toSing (fromType t)) of
    SomeSing (sb :: Sing t) -> withSingI sb
                   (let (a :: DBusValue (TypeArray t))
                            = DBVArray $ catMaybes (dbusValue . fromValue
                                                       <$> (toList v))
                    in DBV a)
fromValue (DBus.ValueBytes bs) = DBV $ DBVByteArray bs
fromValue (DBus.ValueStructure xs) = case valuesToStruct xs of
    (SDBS s) -> DBV (DBVStruct s)
fromValue (DBus.ValueMap kt vt xs) = case fromType kt of
    DBusSimpleType kt' -> case toSing kt' of
        SomeSing (ktS :: Sing ktt) -> case toSing (fromType vt) of
            SomeSing (vtS :: Sing vtt) -> withSingI ktS $
                                          withSingI vtS $
                let (d :: DBusValue (TypeDict ktt vtt))
                        = DBVDict . catMaybes
                          . map (\(k,v) -> liftA2 (,) (dbusValue $ fromAtom k)
                                                      (dbusValue $ fromValue v))
                            $ Map.toList xs
                in DBV d
    _ -> error "fromValue: ValueDict with non-simple key type"


valuesToStruct :: [DBus.Value] -> SomeDBusStruct
valuesToStruct [] = error "valuesToStruct: empty list"
valuesToStruct [x] = case fromValue x of
                         (DBV s) -> SDBS (StructSingleton s)
valuesToStruct (x:xs) = case fromValue x of
                          (DBV s) -> case valuesToStruct xs of
                              (SDBS ss) -> SDBS (StructCons s ss)

-- mkStruct :: [DBus.Variant] -> DBus.Structure
-- mkStruct = unsafeCoerce
