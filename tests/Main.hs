{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Applicative ((<$>), (<*>))
import           DBus.Introspect
import           Text.Hamlet.XML
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Either
import           DBus.Signature

import           DBus.ExtTypes

import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Tasty.TH

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
    shrink _ = []

resized :: Gen a -> Gen a
resized g = sized (\i -> resize (i `div` 2) g)

instance Arbitrary DBusType where
    arbitrary = oneof [ DBusSimpleType <$> resized arbitrary
                      , TypeArray <$> resized arbitrary
                      , TypeStruct <$> resized arbitrary
                      , TypeDict <$> resized arbitrary <*> resized arbitrary
                      , return TypeVariant
                      ]
    shrink (TypeStruct ts) = TypeStruct <$> shrink ts
    shrink (TypeDict kt vt) = (TypeDict kt <$> (shrink vt))
                              ++ (TypeDict <$> (shrink kt) <*> [vt])
    shrink (TypeDictEntry kt vt) = (TypeDictEntry kt <$> (shrink vt))
                                   ++ (TypeDictEntry <$> (shrink kt) <*> [vt])
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

testing_pickler1 = isRight $ xmlToNode nodeXml
testing_pickler2 = let Right node = xmlToNode nodeXml
                       reXml = nodeToXml node
                       Right reNode = xmlToNode reXml
                   in node == reNode


main = $defaultMainGenerator
