{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}

module DBus.Introspect where

import           Blaze.ByteString.Builder
import           Control.Applicative ((<$>))
import           Control.Exception (SomeException)
import qualified Data.ByteString as BS
import           Data.Conduit (($$), ($=))
import           Data.Conduit.List (consume, sourceList)
import           Data.Data(Data)
import           Data.Functor.Identity
import           Data.Maybe
import           Data.Monoid (mconcat)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LText
import           Data.Typeable(Typeable)
import           Data.XML.Pickle hiding (Result)
import           Data.XML.Types
import           Text.XML.Stream.Parse
import           Text.XML.Stream.Render
import           Text.XML.Unresolved (toEvents, fromEvents)

import           DBus.Object
import           DBus.Representable
import           DBus.Signature
import           DBus.Types

data IDirection = In | Out deriving (Eq, Show, Data, Typeable)

directionFromText :: Text.Text -> Either Text.Text IDirection
directionFromText "in" = Right In
directionFromText "out" = Right Out
directionFromText d = Left $ "Not a direction: " `Text.append` d

directionToText :: IDirection -> Text.Text
directionToText In = "in"
directionToText Out = "out"

data IPropertyAccess = Read
                     | Write
                     | ReadWrite
                     deriving (Eq, Show, Data, Typeable)

propertyAccessFromText :: Text.Text -> Either Text.Text IPropertyAccess
propertyAccessFromText "read" = Right Read
propertyAccessFromText "write" = Right Write
propertyAccessFromText "readwrite" = Right ReadWrite
propertyAccessFromText a = Left $ "Not a property access type: " `Text.append` a

propertyAccessToText :: IPropertyAccess -> Text.Text
propertyAccessToText Read = "read"
propertyAccessToText Write = "write"
propertyAccessToText ReadWrite = "readwrite"

data IArgument = IArgument { iArgumentName :: Text.Text
                           , iArgumentType :: DBusType
                           , iArgumentDirection :: Maybe IDirection
                           } deriving (Eq, Show, Data, Typeable)

data IMethod = IMethod { iMethodName :: Text.Text
                       , iMethodArguments :: [IArgument]
                       , iMethodAnnotations :: [Annotation]
                       } deriving (Eq, Show, Data, Typeable)

data ISignal = ISignal { iSignalName :: Text.Text
                       , iSignalArguments :: [IArgument]
                       , iSignalAnnotations :: [Annotation]
                     } deriving (Eq, Show, Data, Typeable)

data IProperty = IProperty { iPropertyName :: Text.Text
                           , iPropertyType :: DBusType
                           , iPropertyAccess :: IPropertyAccess
                           , iPropertyAnnotation :: [Annotation]
                           } deriving (Eq, Show, Data, Typeable)

data IInterface = IInterface { iInterfaceName :: Text.Text
                             , iInterfaceMethods :: [IMethod]
                             , iInterfaceSignals :: [ISignal]
                             , iInterfaceProperties :: [IProperty]
                             , iInterfaceAnnotations :: [Annotation]
                             } deriving (Eq, Show, Data, Typeable)

data INode = INode { nodeName :: Text.Text
                   , nodeInterfaces :: [IInterface]
                   , nodeSubnodes :: [INode]
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

xpDirection :: PU Text.Text IDirection
xpDirection = xpPartial directionFromText directionToText

xpPropertyAccess = xpPartial propertyAccessFromText propertyAccessToText

xpArgument :: PU [Node] IArgument
xpArgument = xpWrap (\(name, tp, dir) -> IArgument name tp dir)
                    (\(IArgument name tp dir) -> (name, tp, dir)) $
                 xpElemAttrs "arg"
                     (xp3Tuple (xpAttribute "name" xpText)
                               (xpAttribute "type" xpSignature)
                               (xpAttribute' "direction" xpDirection)
                     )

xpMethod :: PU [Node] IMethod
xpMethod = xpWrap (\(name,(args, anns)) -> IMethod name args anns )
                  (\(IMethod name args anns) -> (name,(args, anns)) ) $
               xpElem "method"
                    (xpAttribute "name" xpText)
                    (xp2Tuple (xpFindMatches xpArgument)
                              (xpFindMatches xpAnnotation))

xpSignal :: PU [Node] ISignal
xpSignal = xpWrap (\(name, (args, anns)) -> ISignal name args anns )
                  (\(ISignal name args anns) -> (name, (args, anns)) ) $
               xpElem "signal"
                    (xpAttribute "name" xpText)
                    (xp2Tuple (xpFindMatches xpArgument)
                              (xpFindMatches xpAnnotation))

xpProperty :: PU [Node] IProperty
xpProperty = xpWrap (\((name, tp, access), anns)
                         -> IProperty name tp access anns)
                    (\(IProperty name tp access anns)
                         -> ((name, tp, access), anns)) $
                 xpElem "property"
                     (xp3Tuple (xpAttribute "name" xpText)
                               (xpAttribute "type" xpSignature)
                               (xpAttribute "access" xpPropertyAccess))
                     (xpFindMatches xpAnnotation)


xpInterface :: PU [Node] IInterface
xpInterface = xpWrap (\(name, (methods, signals, properties, annotations))
                        -> IInterface name methods signals properties annotations)
                     (\(IInterface name methods signals properties annotations)
                          -> (name, (methods, signals, properties, annotations))) $
                  xpElem "interface"
                      (xpAttribute "name" xpText)
                      (xp4Tuple (xpFindMatches xpMethod)
                                (xpFindMatches xpSignal)
                                (xpFindMatches xpProperty)
                                (xpFindMatches xpAnnotation) )

xpNode :: PU [Node] INode
xpNode = xpWrap (\(name, (is, ns)) -> INode name is ns)
                (\(INode name is ns) -> (name, (is, ns))) $
            xpElem "node"
                (xpAttribute "name" xpText)
                (xp2Tuple (xpFindMatches xpInterface)
                          (xpFindMatches xpNode))

xmlToNode :: BS.ByteString -> Either Text.Text INode
xmlToNode xml = case sourceList [xml] $= parseBytesPos def $$ fromEvents of
    Left e -> Left (Text.pack $ show (e :: SomeException))
    Right d -> case unpickle (xpRoot . xpUnliftElems $ xpNode) $ documentRoot d of
        Left e -> Left $ Text.pack (ppUnpickleError e)
        Right r -> Right r


pubID :: ExternalID
pubID = PublicID "-//freedesktop//DTD D-BUS Object Introspection 1.0//EN"
                 "http://www.freedesktop.org/standards/dbus/1.0/introspect.dtd"

prologue :: Prologue
prologue = Prologue { prologueBefore = []
                    , prologueDoctype =
                           Just (Doctype {doctypeName = "node"
                                         , doctypeID =
                                             Just pubID
                                         })
                    , prologueAfter = []}


nodeToXml :: INode -> BS.ByteString
nodeToXml node = toByteString . mconcat . runIdentity $
                 (sourceList (toEvents doc)
                  $= renderBuilder def
                  $$ consume)
  where
    doc = Document prologue (pickle (xpRoot . xpUnliftElems $ xpNode) node) []

introspectMethods :: [Method] -> [IMethod]
introspectMethods = map introspectMethod
  where
    introspectMethod m = IMethod (methodName m) (toArgs m) []
    toArgs m@(Method _ _ ds) =
        let (args, res) = methodSignature m
            (ts, rs) = argDescriptions ds
        in zipWith (\n t -> IArgument n t (Just In)) ts args
           ++ zipWith ((\r t -> IArgument r t (Just Out))) rs res


introspectSignalArgument a =
    IArgument { iArgumentName = signalArgumentName a
              , iArgumentType = signalArgumentType a
              , iArgumentDirection = Nothing
              }

introspectSignal s =
    ISignal { iSignalName = signalName s
            , iSignalArguments = introspectSignalArgument <$> signalArguments s
            , iSignalAnnotations = signalAnnotations s
            }

propertyAccess (Property{propertyAccessors = PropertyWrapper mbSet mbGet})
    = case (mbSet, mbGet) of
    (Just{}, Just{}) -> ReadWrite
    (Nothing, Just{}) -> Read
    (Just{}, Nothing) -> Write
    (Nothing, Nothing) -> error "iPropertyAccess: Both getter and setter are nothing"


introspectProperty p = IProperty { iPropertyName = propertyName p
                                 , iPropertyType = propertyType p
                                 , iPropertyAccess = propertyAccess p
                                 , iPropertyAnnotation = [] -- TODO
                                 }

introspectInterface :: Interface -> IInterface
introspectInterface i = IInterface { iInterfaceName = interfaceName i
                                   , iInterfaceMethods = introspectMethods
                                                           $ interfaceMethods i
                                   , iInterfaceSignals =
                                       introspectSignal <$> interfaceSignals i
                                   , iInterfaceProperties =
                                       introspectProperty
                                         <$> interfaceProperties i
                                   , iInterfaceAnnotations = [] -- TODO
                                   }

introspectObject o = INode { nodeName = objectPathToText $ objectObjectPath o
                           , nodeInterfaces = introspectInterface <$>
                                                 objectInterfaces o
                           , nodeSubnodes = introspectObject
                                              <$> objectSubObjects o
                           }

introspect :: Object -> IO Text.Text
introspect object = return $ Text.decodeUtf8 . nodeToXml $ introspectObject object

introspectMethod :: Object -> Method
introspectMethod object = Method (repMethod $ introspect object)
                                  "Introspect"
                                  (Result ("xml_data" :> ResultNil))

introspectable :: Object -> Interface
introspectable o = Interface{ interfaceName = "org.freedesktop.DBus.Introspectable"
                            , interfaceMethods = [introspectMethod o]
                            , interfaceSignals = []
                            , interfaceAnnotations = []
                            , interfaceProperties = []
                            }

addIntrospectable :: Object -> Object
addIntrospectable o@(Object nm is sos) =
    let intr = introspectable o
    in Object nm (if intr `elem` is then is else (intr:is))
                 (addIntrospectable <$> sos)
