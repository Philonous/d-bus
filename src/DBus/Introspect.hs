{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module DBus.Introspect where

import           Blaze.ByteString.Builder
import           Control.Applicative ((<$>))
import           Control.Exception (SomeException)
import           Data.Functor.Identity
import qualified Data.ByteString as BS
import           Data.Conduit (($$), ($=))
import           Data.Conduit.List (consume, sourceList)
import           Data.Data(Data)
import           Data.Monoid (mconcat)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LText
import           Data.Typeable(Typeable)
import           Data.XML.Pickle
import           Data.XML.Types
import           Text.XML.Stream.Parse
import           Text.XML.Stream.Render
import           Text.XML.Unresolved (toEvents, fromEvents)

import           DBus.ExtTypes
import           DBus.Signature

data Direction = In | Out deriving (Eq, Show, Data, Typeable)

directionFromText :: Text.Text -> Either Text.Text Direction
directionFromText "in" = Right In
directionFromText "out" = Right Out
directionFromText d = Left $ "Not a direction: " `Text.append` d

directionToText :: Direction -> Text.Text
directionToText In = "in"
directionToText Out = "out"

data PropertyAccess = Read | Write | ReadWrite deriving (Eq, Show, Data, Typeable)

propertyAccessFromText :: Text.Text -> Either Text.Text PropertyAccess
propertyAccessFromText "read" = Right Read
propertyAccessFromText "write" = Right Write
propertyAccessFromText "readwrite" = Right ReadWrite
propertyAccessFromText a = Left $ "Not a property access type: " `Text.append` a

propertyAccessToText :: PropertyAccess -> Text.Text
propertyAccessToText Read = "read"
propertyAccessToText Write = "write"
propertyAccessToText ReadWrite = "readwrite"

data Annotation = Annotation { annotationName :: Text.Text
                             , annotationValue :: Text.Text
                             } deriving (Eq, Show, Data, Typeable)

data Argument = Argument { argumentName :: Text.Text
                         , argumentType :: DBusType
                         , argumentDirection :: Maybe Direction
                         } deriving (Eq, Show, Data, Typeable)

data Method = Method { methodName :: Text.Text
                     , methodArguments :: [Argument]
                     , methodAnnotations :: [Annotation]
                     } deriving (Eq, Show, Data, Typeable)

data Signal = Signal { signalName :: Text.Text
                     , signalArguments :: [Argument]
                     , signalAnnotations :: [Annotation]
                     } deriving (Eq, Show, Data, Typeable)

data Property = Property { propertyName :: Text.Text
                         , propertType :: DBusType
                         , propertyAccess :: PropertyAccess
                         , propertyAnnotation :: [Annotation]
                         } deriving (Eq, Show, Data, Typeable)

data Interface = Interface { interfaceName :: Text.Text
                           , interfaceMethods :: [Method]
                           , interfaceSignals :: [Signal]
                           , interfaceProperties :: [Property]
                           , interfaceAnnotations :: [Annotation]
                           } deriving (Eq, Show, Data, Typeable)

data DBusNode = DBusNode { nodeName :: Text.Text
                         , nodeInterfaces :: [Interface]
                         , nodeSubnodes :: [DBusNode]
                         } deriving (Eq, Show, Data, Typeable)

xpAnnotation :: PU [Node] Annotation
xpAnnotation = xpWrap (\(name, content) -> Annotation name content)
                      (\(Annotation name content) -> (name, content))  $
                      xpElemAttrs "annotation"
                          (xp2Tuple (xpAttribute "name" xpText)
                                    (xpAttribute "value" xpText))

xpSignature :: PU Text.Text DBusType
xpSignature = xpPartial (eitherParseSig . Text.encodeUtf8)
                        (Text.decodeUtf8 . toSignature)

xpDirection :: PU Text.Text Direction
xpDirection = xpPartial directionFromText directionToText

xpPropertyAccess = xpPartial propertyAccessFromText propertyAccessToText

xpArgument :: PU [Node] Argument
xpArgument = xpWrap (\(name, tp, dir) -> Argument name tp dir)
                    (\(Argument name tp dir) -> (name, tp, dir)) $
                 xpElemAttrs "arg"
                     (xp3Tuple (xpAttribute "name" xpText)
                               (xpAttribute "type" xpSignature)
                               (xpAttribute' "direction" xpDirection)
                     )

xpMethod :: PU [Node] Method
xpMethod = xpWrap (\(name,(args, anns)) -> Method name args anns )
                  (\(Method name args anns) -> (name,(args, anns)) ) $
               xpElem "method"
                    (xpAttribute "name" xpText)
                    (xp2Tuple (xpFindMatches xpArgument)
                              (xpFindMatches xpAnnotation))

xpSignal :: PU [Node] Signal
xpSignal = xpWrap (\(name, (args, anns)) -> Signal name args anns )
                  (\(Signal name args anns) -> (name, (args, anns)) ) $
               xpElem "signal"
                    (xpAttribute "name" xpText)
                    (xp2Tuple (xpFindMatches xpArgument)
                              (xpFindMatches xpAnnotation))

xpProperty = xpWrap (\((name, tp, access), anns)
                         -> Property name tp access anns)
                    (\(Property name tp access anns)
                         -> ((name, tp, access), anns)) $
                 xpElem "property"
                     (xp3Tuple (xpAttribute "name" xpText)
                               (xpAttribute "type" xpSignature)
                               (xpAttribute "access" xpPropertyAccess))
                     (xpFindMatches xpAnnotation)


xpInterface :: PU [Node] Interface
xpInterface = xpWrap (\(name, (methods, signals, properties, annotations))
                        -> Interface name methods signals properties annotations)
                     (\(Interface name methods signals properties annotations)
                          -> (name, (methods, signals, properties, annotations))) $
                  xpElem "interface"
                      (xpAttribute "name" xpText)
                      (xp4Tuple (xpFindMatches xpMethod)
                                (xpFindMatches xpSignal)
                                (xpFindMatches xpProperty)
                                (xpFindMatches xpAnnotation) )

xpNode :: PU [Node] DBusNode
xpNode = xpWrap (\(name, (is, ns)) -> DBusNode name is ns)
                (\(DBusNode name is ns) -> (name, (is, ns))) $
            xpElem "node"
                (xpAttribute "name" xpText)
                (xp2Tuple (xpFindMatches xpInterface)
                          (xpFindMatches xpNode))

xmlToNode xml = case sourceList [xml] $= parseBytesPos def $$ fromEvents of
    Left e -> Left (Text.pack $ show (e :: SomeException))
    Right d -> case unpickle (xpRoot . xpUnliftElems $ xpNode) $ documentRoot d of
        Left e -> Left $ Text.pack (ppUnpickleError e)
        Right r -> Right r


pubID = PublicID "-//freedesktop//DTD D-BUS Object Introspection 1.0//EN"
                 "http://www.freedesktop.org/standards/dbus/1.0/introspect.dtd"

prologue = Prologue { prologueBefore = []
                    , prologueDoctype =
                           Just (Doctype {doctypeName = "node"
                                         , doctypeID =
                                             Just pubID
                                         })
                    , prologueAfter = []}


nodeToXml node = toByteString . mconcat . runIdentity $
                 (sourceList (toEvents doc)
                  $= renderBuilder def
                  $$ consume)
  where
    doc = Document prologue (pickle (xpRoot . xpUnliftElems $ xpNode) node) []
