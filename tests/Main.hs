{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad
import           Control.Monad.Reader
import           DBus.Signature
import           Data.Binary.Get
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Builder as BS
import           Data.Char
import           Data.Either
import           Data.Int
import           Data.List (intercalate)
import           Data.Singletons
import           Data.Singletons.Prelude.List
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Word

import           DBus.Types
import           DBus.Wire

-- import           DBus.Introspect
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Tasty.TH

import           Numeric
import           Debug.Trace

instance Arbitrary DBusSimpleType where
    arbitrary = oneof [ return TypeByte
                      , return TypeBoolean
                      , return TypeInt16
                      , return TypeUInt16
                      , return TypeInt32
                      , return TypeUInt32
                      , return TypeInt64
                      , return TypeUInt64
                      , return TypeDouble
                      , return TypeUnixFD
                      , return TypeString
                      , return TypeObjectPath
                      , return TypeSignature
                      ]
    shrink _ = [TypeByte]

resized :: Gen a -> Gen a
resized g = sized (\i -> resize (i `div` 2) g)

maxsized :: Int -> Gen a -> Gen a
maxsized s g = sized (\i -> resize (min s i) g)

instance Arbitrary DBusType where
    arbitrary = oneof [ DBusSimpleType <$> resized arbitrary
                      , TypeArray <$>  resized arbitrary
                      , TypeStruct <$> resized (listOf1 arbitrary)
                      , TypeDict <$> resized arbitrary <*> resized arbitrary
                      , return TypeVariant
                      ]
    shrink (TypeStruct ts) = TypeStruct <$> (filter (not.null) $ shrink ts)
    shrink (TypeDict kt vt) = (TypeDict kt <$> (shrink vt))
                              ++ (TypeDict <$> (shrink kt) <*> [vt])
    shrink (TypeDictEntry kt vt) = (TypeDictEntry kt <$> (shrink vt))
                                   ++ (TypeDictEntry <$> (shrink kt) <*> [vt])
    shrink (TypeArray t) = TypeArray <$> shrink t
    shrink t = []

prop_signatureInverse = \s -> eitherParseSig (toSignature s) == Right s

nodeXml = Text.encodeUtf8 $ Text.concat
    [ "<!DOCTYPE node PUBLIC \"-//freedesktop//DTD D-BUS Object Introspection \
            \1.0//EN\""
    , "  \"http://www.freedesktop.org/standards/dbus/1.0/introspect.dtd\">"
    , " <node name=\"/com/example/sample_object\">"
    , "   <interface name=\"com.example.SampleInterface\">"
    , "     <method name=\"Frobate\">"
    , "       <arg name=\"foo\" type=\"i\" direction=\"in\"/>"
    , "       <arg name=\"bar\" type=\"s\" direction=\"out\"/>"
    , "       <arg name=\"baz\" type=\"a{us}\" direction=\"out\"/>"
    , "       <annotation name=\"org.freedesktop.DBus.Deprecated\" value=\"true\"/>"
    , "     </method>"
    , "     <method name=\"Bazify\">"
    , "       <arg name=\"bar\" type=\"(iiu)\" direction=\"in\"/>"
    , "       <arg name=\"bar\" type=\"v\" direction=\"out\"/>"
    , "     </method>"
    , "     <method name=\"Mogrify\">"
    , "       <arg name=\"bar\" type=\"(iiav)\" direction=\"in\"/>"
    , "     </method>"
    , "     <signal name=\"Changed\">"
    , "       <arg name=\"new_value\" type=\"b\"/>"
    , "     </signal>"
    , "     <property name=\"Bar\" type=\"y\" access=\"readwrite\"/>"
    , "   </interface>"
    , "   <node name=\"child_of_sample_object\"/>"
    , "   <node name=\"another_child_of_sample_object\"/>"
    , "</node>"
    ]

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _) = False

-- testing_pickler1 = isRight $ xmlToNode nodeXml
-- testing_pickler2 = let Right node = xmlToNode nodeXml
--                        reXml = nodeToXml node
--                        Right reNode = xmlToNode reXml
--                    in node == reNode


genText = Text.pack <$> arbitrary
genOP = objectPath . Text.pack <$> arbitrary


shrinkTypes [x] = (:[]) <$> shrinkType x
shrinkTypes xs = if BS.length (toSignatures xs) > 255
                 then shrinkTypes (tail xs)
                 else return xs

genDBV :: Sing t -> Gen (DBusValue t)
genDBV (SDBusSimpleType STypeByte)       = DBVByte       <$> arbitrary
genDBV (SDBusSimpleType STypeBoolean)    = DBVBool       <$> arbitrary
genDBV (SDBusSimpleType STypeInt16)      = DBVInt16      <$> arbitrary
genDBV (SDBusSimpleType STypeUInt16)     = DBVUInt16     <$> arbitrary
genDBV (SDBusSimpleType STypeInt32)      = DBVInt32      <$> arbitrary
genDBV (SDBusSimpleType STypeUInt32)     = DBVUInt32     <$> arbitrary
genDBV (SDBusSimpleType STypeInt64)      = DBVInt64      <$> arbitrary
genDBV (SDBusSimpleType STypeUInt64)     = DBVUInt64     <$> arbitrary
genDBV (SDBusSimpleType STypeDouble)     = DBVDouble     <$> arbitrary
genDBV (SDBusSimpleType STypeUnixFD)     = DBVUnixFD     <$> arbitrary
genDBV (SDBusSimpleType STypeString)     = DBVString     <$> genText
genDBV (SDBusSimpleType STypeObjectPath) = DBVObjectPath <$> genOP
genDBV (SDBusSimpleType STypeSignature)  = maxsized 10 $
    DBVSignature  <$> (shrinkTypes =<< arbitrary)
genDBV STypeVariant                      = resized $ do
    t <- maxsized 10 $ shrinkType =<< arbitrary :: Gen DBusType
    case toSing t of
        SomeSing (st :: Sing t) -> withSingI st $ DBVVariant <$> genDBV st
genDBV (STypeArray t)                    = resized $ DBVArray
                                               <$> listOf (genDBV t)
genDBV (STypeStruct ts) = resized $ DBVStruct <$> genStruct ts
genDBV (STypeDict kt vt) = resized $
    DBVDict <$> listOf ((,) <$> genDBV (SDBusSimpleType kt)
                            <*> genDBV vt)


genStruct :: Sing ts -> Gen (DBusStruct ts)
genStruct (SCons t SNil) = StructSingleton <$> genDBV t
genStruct (SCons t ts) = StructCons <$> genDBV t <*> genStruct ts

for = flip map

shrinkType t = if BS.length (toSignature t) > 255
               then shrinkType =<< elements (shrink t)
               else return t

instance Arbitrary SomeDBusValue where
    arbitrary = do
        t <- shrinkType =<< arbitrary :: Gen DBusType
        case toSing t of
            SomeSing st -> withSingI st $ DBV <$> genDBV st
    shrink (DBV x) = case x of
        DBVVariant y -> (DBV y)
                        : ((\(DBV x) -> DBV (DBVVariant x)) <$> shrink (DBV y))
        (DBVArray (a:as) :: DBusValue t) -> case (sing :: Sing t) of
            STypeArray ts -> withSingI ts $
                ( case shrink (fromSing (sing :: Sing t)) of
                       tss -> for tss $ \tshrink -> case toSing tshrink of
                               (SomeSing (ss :: Sing (tt :: DBusType)))
                                   -> withSingI ss $
                                   DBV (DBVArray ([] :: [DBusValue tt]))
                ) ++
                [ DBV ( DBVArray [] :: DBusValue t)
                , DBV ( DBVArray as :: DBusValue t)
                , DBV ( DBVArray (init (a:as)) :: DBusValue t)
                , case (sing :: Sing t) of
                       STypeArray st
                           -> withSingI st $ DBV a
                ] ++ map (\(DBV b) -> DBV (DBVArray [b])) (shrink (DBV a))

        (DBVDict ((k,v):as) :: DBusValue t)
            -> [ DBV ( DBVDict  [] :: DBusValue t)
               , DBV ( DBVDict  as :: DBusValue t)
               , case (sing :: Sing t) of
                      STypeDict kt vt -> withSingI kt $ DBV  k
               , case (sing :: Sing t) of
                      STypeDict kt vt -> withSingI vt $ DBV  v
               ]

        (DBVStruct fs :: DBusValue t) ->
            case (sing :: Sing t) of
                STypeStruct ts -> shrinkStruct ts fs
        _ -> []

shrinkStruct :: Sing ts -> DBusStruct ts -> [SomeDBusValue]
shrinkStruct (SCons t SNil) (StructSingleton x) = withSingI t [DBV x]
shrinkStruct (SCons t ts) (StructCons x xs) =
    withSingI t $ withSingI ts $
    (DBV $ DBVStruct xs)
    : (DBV x)
    : (shrinkStruct ts xs)

hexifyChar c = case showHex c "" of
    [x] -> ['0',x]
    x -> x

hexifyBS bs = intercalate " " $ hexifyChar <$> BSL.unpack bs

showifyChar c = if ord 'a' <= c && c <= ord 'z'
                then chr c : " "
                else "  "

showifyBS bs = intercalate " " $ showifyChar . fromIntegral <$> BSL.unpack bs

-- sho bs = "============\n" ++ show bs ++ "\n -------------- \n" ++ hexifyBS bs
sho bs = show bs

to :: SingI t => DBusValue t -> BSL.ByteString
to x = (BS.toLazyByteString $ runDBusPut Big (putDBV x))


wire_inverse (x :: DBusValue t) =
    let from = runGet (runReaderT getDBV Big) (to x)
    in  (DBV from, x == from)

prop_wire_inverse (DBV x) = snd $ wire_inverse x


main = $defaultMainGenerator

ppv (x :: DBusValue t) = do
    print x
    let bs = to x
    return $! bs
    forM_ [1..32] $ \i -> (putStr $ hexifyChar i) >> putStr " "
    putStrLn ""
    putStrLn (hexifyBS bs)
    putStrLn (showifyBS bs)
    putStrLn (" ## " ++ show (BSL.length bs))
    let from = runGet (runReaderT getDBV Big) bs
    print from
    print x
    print $ x == from

foo = sample' arbitrary >>= (mapM_ $ \(DBV x) -> ppv x)

test1 = DBVVariant (DBVArray [DBVUInt16 2])
test2 = DBVArray [DBVByte 0 , DBVByte 3, DBVByte 0]
test3 = DBVVariant (DBVVariant (DBVStruct (StructSingleton (DBVStruct (StructSingleton (DBVUInt16 9))))))

test4 = DBVArray [DBVVariant $ DBVUInt32 7 ]
test5 = DBVArray [DBVVariant $ DBVUInt64 7 ]

test6 = DBVArray [DBVStruct (StructSingleton (DBVVariant (DBVUInt32 0)))]
test7 = DBVArray [DBVStruct (StructSingleton (DBVUInt64 (maxBound :: Word64)))]

test8o = DBVVariant (DBVVariant (DBVArray [DBVVariant (DBVVariant (DBVInt16 (-1)))]))

test8 = DBVVariant (DBVVariant (DBVArray [DBVVariant (DBVVariant (DBVInt16 (-1)))]))
