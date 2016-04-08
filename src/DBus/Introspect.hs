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
import           Control.Monad
import qualified Data.ByteString as BS
import           Data.Conduit (($$), ($=))
import           Data.Conduit.List (consume, sourceList)
import           Data.Data (Data)
import           Data.Functor.Identity
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Monoid (mconcat)
import           Data.Singletons
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LText
import           Data.Typeable (Typeable)
import           Data.XML.Pickle hiding (Result)
import           Data.XML.Types
import           Text.XML.Stream.Parse
import           Text.XML.Stream.Render
import           Text.XML.Unresolved (toEvents, fromEvents)

import           DBus.Object
import           DBus.Representable
import           DBus.Signature
import           DBus.Types
import           DBus.Method

data IDirection = In | Out deriving (Eq, Show, Data, Typeable)

introspectableInterfaceName :: Text
introspectableInterfaceName = "org.freedesktop.DBus.Introspectable"

directionFromText :: Text -> Either Text IDirection
directionFromText "in" = Right In
directionFromText "out" = Right Out
directionFromText d = Left $ "Not a direction: " `Text.append` d

directionToText :: IDirection -> Text
directionToText In = "in"
directionToText Out = "out"


propertyAccessFromText :: Text -> Either Text PropertyAccess
propertyAccessFromText "read" = Right Read
propertyAccessFromText "write" = Right Write
propertyAccessFromText "readwrite" = Right ReadWrite
propertyAccessFromText a = Left $ "Not a property access type: " `Text.append` a

propertyAccessToText :: PropertyAccess -> Text
propertyAccessToText Read = "read"
propertyAccessToText Write = "write"
propertyAccessToText ReadWrite = "readwrite"

data IArgument = IArgument { iArgumentName :: Text
                           , iArgumentType :: DBusType
                           , iArgumentDirection :: Maybe IDirection
                           } deriving (Eq, Show, Data, Typeable)

data IMethod = IMethod { iMethodName :: Text
                       , iMethodArguments :: [IArgument]
                       , iMethodAnnotations :: [Annotation]
                       } deriving (Eq, Show, Data, Typeable)

data ISignal = ISignal { iSignalName :: Text
                       , iSignalArguments :: [IArgument]
                       , iSignalAnnotations :: [Annotation]
                     } deriving (Eq, Show, Data, Typeable)

data IProperty = IProperty { iPropertyName :: Text
                           , iPropertyType :: DBusType
                           , iPropertyAccess :: PropertyAccess
                           , iPropertyAnnotation :: [Annotation]
                           } deriving (Eq, Show, Data, Typeable)

data IInterface = IInterface { iInterfaceName :: Text
                             , iInterfaceMethods :: [IMethod]
                             , iInterfaceSignals :: [ISignal]
                             , iInterfaceProperties :: [IProperty]
                             , iInterfaceAnnotations :: [Annotation]
                             } deriving (Eq, Show, Data, Typeable)

data INode = INode { nodeName :: Maybe Text
                   , nodeInterfaces :: [IInterface]
                   , nodeSubnodes :: [INode]
                   } deriving (Eq, Show, Data, Typeable)

xpAnnotation :: PU [Node] Annotation
xpAnnotation = xpWrap (\(name, content) -> Annotation name content)
                      (\(Annotation name content) -> (name, content))  $
                      xpElemAttrs "annotation"
                          (xp2Tuple (xpAttribute "name" xpText)
                                    (xpAttribute "value" xpText))

xpSignature :: PU Text DBusType
xpSignature = xpPartial (eitherParseSig . Text.encodeUtf8)
                        (Text.decodeUtf8 . toSignature)

xpDirection :: PU Text IDirection
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
                (xpAttribute' "name" xpText)
                (xp2Tuple (xpFindMatches xpInterface)
                          (xpFindMatches xpNode))

xmlToNode :: BS.ByteString -> Either Text INode
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
    toArgs m@(Method _ _ argDs resDs) =
        let (args, res) = methodSignature m
            (ts, rs) = argDescriptions argDs resDs
        in zipWith (\n t -> IArgument n t (Just In)) ts args
           ++ zipWith ((\r t -> IArgument r t (Just Out))) rs res


introspectSignalArgument :: DBusType -> Text -> IArgument
introspectSignalArgument tp name =
    IArgument { iArgumentName = name
              , iArgumentType = tp
              , iArgumentDirection = Nothing
              }

introspectSignal :: SomeSignalDescription -> ISignal
introspectSignal (SSD (s :: SignalDescription a)) =
    ISignal { iSignalName = signalDMember s
            , iSignalArguments = zipWith introspectSignalArgument
                                    (fromSing $ (sing :: Sing a))
                                    (adToList $ signalDArguments s)

            , iSignalAnnotations = [] -- signalAnnotations s
            }

propertyAccess :: Property t -> PropertyAccess
propertyAccess Property{propertySet = mbSet, propertyGet = mbGet}
    = case (mbSet, mbGet) of
    (Just{}, Just{}) -> ReadWrite
    (Nothing, Just{}) -> Read
    (Just{}, Nothing) -> Write
    (Nothing, Nothing) -> error "iPropertyAccess: Both getter and setter are Nothing"


introspectProperty :: SomeProperty -> IProperty
introspectProperty (SomeProperty p) =
    IProperty { iPropertyName = propertyName p
              , iPropertyType = propertyType p
              , iPropertyAccess = propertyAccess p
              , iPropertyAnnotation = [] -- TODO
                                 }

introspectInterface :: Text -> Interface -> IInterface
introspectInterface n i = IInterface { iInterfaceName = n
                                     , iInterfaceMethods = introspectMethods
                                                           $ interfaceMethods i
                                     , iInterfaceSignals =
                                         introspectSignal <$> interfaceSignals i
                                     , iInterfaceProperties =
                                         introspectProperty
                                         <$> interfaceProperties i
                                     , iInterfaceAnnotations = [] -- TODO
                                     }


-- | Delete prefix from map, returns (elements with prefix delete, elements that
-- didn't have this prefix)
deletePrefix :: ObjectPath -> Map ObjectPath a -> ( Map ObjectPath a
                                                  , Map ObjectPath a)
deletePrefix pre m =
    let (sub, rest) = Map.partitionWithKey (\k _ -> pre `isPathPrefix` k) m
    in (Map.mapKeys (fromJust . stripObjectPrefix pre) sub, rest)


grabPrefixes :: Map ObjectPath a -> [(ObjectPath, a, Map ObjectPath a)]
grabPrefixes m =
    let mbMin = Map.minViewWithKey m
    in case mbMin of
        Nothing -> []
        Just ((pre, obj), m')
            -> let (sub, rest) = deletePrefix pre m'
               in (pre, obj, sub) : grabPrefixes rest

propertiesInterfaceName :: Text
propertiesInterfaceName = "org.freedesktop.DBus.Properties"

propertiesInterface :: IInterface
propertiesInterface =
    IInterface {
        iInterfaceName = "org.freedesktop.DBus.Properties"
      , iInterfaceMethods =
          [IMethod{
               iMethodName = "Get"
             , iMethodArguments =
                 [ IArgument {
                       iArgumentName = "interface_name"
                       , iArgumentType = DBusSimpleType TypeString
                       , iArgumentDirection = Just In}
                 , IArgument {
                       iArgumentName = "property_name"
                     , iArgumentType = DBusSimpleType TypeString
                     , iArgumentDirection = Just In}
                          , IArgument {
                                iArgumentName = "value"
                              , iArgumentType = TypeVariant
                              , iArgumentDirection = Just Out }]
                      , iMethodAnnotations = []}
          , IMethod{
                iMethodName = "Set"
              , iMethodArguments =
                  [ IArgument {
                         iArgumentName = "interface_name"
                       , iArgumentType = DBusSimpleType TypeString
                       , iArgumentDirection = Just In }
                  , IArgument {
                         iArgumentName = "property_name"
                       , iArgumentType = DBusSimpleType TypeString
                       , iArgumentDirection = Just In }
                  , IArgument {
                         iArgumentName = "value"
                        , iArgumentType = TypeVariant
                        , iArgumentDirection = Just In}]
              , iMethodAnnotations = [] }
          , IMethod{
                iMethodName = "GetAll"
              , iMethodArguments =
                  [ IArgument {
                         iArgumentName = "interface_name"
                       , iArgumentType = DBusSimpleType TypeString
                       , iArgumentDirection = Just In }
                  , IArgument {
                         iArgumentName = "props"
                       , iArgumentType = TypeDict TypeString TypeVariant
                       , iArgumentDirection = Just Out }]
              , iMethodAnnotations = [] }]
      , iInterfaceSignals =
              [ ISignal { iSignalName = "PropertiesChanged"
                        , iSignalArguments =
                            [ IArgument {
                                   iArgumentName = "interface_name"
                                 , iArgumentType = DBusSimpleType TypeString
                                 , iArgumentDirection = Nothing}
                            , IArgument {
                                   iArgumentName = "changed_properties"
                                 , iArgumentType = TypeDict TypeString TypeVariant
                                 , iArgumentDirection = Nothing}
                            , IArgument {
                                   iArgumentName = "invalidated_properties"
                                 , iArgumentType =
                                     TypeArray (DBusSimpleType TypeString)
                                 , iArgumentDirection = Nothing}]
                        , iSignalAnnotations = []}]
      , iInterfaceProperties = []
      , iInterfaceAnnotations = []
      }

introspectObject :: Bool
                 -> ObjectPath
                 -> Object
                 -> Map ObjectPath Object
                 -> INode
introspectObject recurse path (Object ifaces) sub
    = INode { nodeName = Just $ objectPathToText path
            , nodeInterfaces =
                let propIface = if hasInterface propertiesInterfaceName ifaces
                                then []
                                else [propertiesInterface]
                    introspectIface =
                        if hasInterface introspectableInterfaceName ifaces
                        then []
                        else [introspectInterface introspectableInterfaceName $
                                introspectableInterface path False undefined]
                in concat [introspectIface
                          , propIface
                          , uncurry introspectInterface <$> Map.toList ifaces
                          ]

            , nodeSubnodes = (if recurse
                              then (uncurry3 $ introspectObject True)
                              else \(n, _, _) ->
                                INode { nodeName = Just $ objectPathToText n
                                      , nodeInterfaces = []
                                      , nodeSubnodes = []
                                      })
                             <$> grabPrefixes sub
            }
  where
    hasInterface :: Text -> Map Text Interface -> Bool
    hasInterface iname o = isJust $ Map.lookup iname o

uncurry3 f (x, y, z) = f x y z

introspectObjects path recursive objs@(Objects os) =
    case Map.updateLookupWithKey (\_ _ -> Nothing) path os of
           (Nothing, _) ->
               let oss = Map.mapKeys (fromMaybe "" . stripObjectPrefix path) os
               in INode { nodeName = Just $ objectPathToText path
                        , nodeInterfaces = []
                        , nodeSubnodes =
                            uncurry3 (introspectObject recursive )
                              <$> grabPrefixes oss
                        }
           (Just o, os') ->
               let oss = Map.mapKeys (fromMaybe "" . stripObjectPrefix path) os'
               in introspectObject recursive path o oss


introspect :: ObjectPath -> Bool -> Objects -> Text
introspect path recursive object = Text.decodeUtf8 . nodeToXml
                                 $ introspectObjects path recursive object

introspectMethod :: ObjectPath -> Bool -> Objects -> Method
introspectMethod path recursive object =
    Method (repMethod $ (return (introspect path recursive object) :: IO Text))
           "Introspect"
           Done
           ("xml_data" :> Done)


introspectableInterface path recursive o =
    Interface{ interfaceMethods = [introspectMethod path recursive o]
             , interfaceSignals = []
             , interfaceAnnotations = []
             , interfaceProperties = []
             }

introspectable :: ObjectPath -> Bool -> Objects -> Object
introspectable path recursive =
    object introspectableInterfaceName . introspectableInterface path recursive

addIntrospectable :: Objects -> Objects
addIntrospectable os =
    let ro = root "/" (Object Map.empty) <> os
        intrs = for (Map.keys (Map.delete "/" (unObjects ro))) $ \path ->
            root path (introspectable path False ro)
        intr = introspectable "/" True ro
    in os <> root "/" intr <> mconcat intrs
  where
    for = flip fmap
