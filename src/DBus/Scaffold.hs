{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

-- | TH helpers to build scaffolding from introspection data

module DBus.Scaffold
  ( module DBus.Scaffold
  , def
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Char
import           Data.Default
import           Data.Maybe (fromMaybe, catMaybes)
import           Data.Monoid
import           Data.Singletons
import           Data.Singletons.Prelude.List
import           Data.Text (Text)
import qualified Data.Text as Text
import           Language.Haskell.TH
import           Language.Haskell.TH.Lib
import           Language.Haskell.TH.Syntax

import           DBus.Introspect
import           DBus.Message
import           DBus.Representable
import           DBus.Types
import           DBus.Property

data DBusEndpointOptions =
  DBusEndpointOptions { methodNames   :: SomeMethodDescription -> Maybe String
                      , propertyNames :: String -> String
                      , signalNames   :: String -> String
                      }

defaultDbusEndpointOptions :: DBusEndpointOptions
defaultDbusEndpointOptions =
    DBusEndpointOptions
    { methodNames  = \(SMD md) -> filterInterfaces (methodInterface md)
                                    . downcase . Text.unpack $ methodMember md
    , propertyNames = downcase . (++ "P")
    , signalNames =  downcase . (++ "Signal")
    }
  where
    filterInterfaces iface x =
      case iface `elem` [ introspectableInterfaceName
                        , "org.freedesktop.DBus.Peer"
                        , "org.freedesktop.DBus.Properties"
                        ] of
       True -> Nothing
       False -> Just x
    downcase [] = []
    downcase (x:xs) = toLower x : xs

instance Default DBusEndpointOptions where
  def = defaultDbusEndpointOptions

makeDbusEndpoints :: DBusEndpointOptions -> ObjectPath -> FilePath -> Q [Dec]
makeDbusEndpoints conf root xmlFile = do -- @TODO: root
  node <- readIntrospectXml xmlFile
  let methods = nodeMethodDescriptions "" node
      propDs = nodePropertyDescriptions "" node
      sigDs = nodeSignals "" node
      downcase [] = []
      downcase (x:xs) = toLower x : xs
  mfs <- fmap catMaybes . forM methods $ \smd ->
            case methodNames conf smd of
             Nothing -> return Nothing
             Just name -> Just <$> liftMethodDescription name smd
  props <- forM propDs $ propertyFromDescription
           (propertyNames conf . Text.unpack . pdName)
           Nothing
  -- sigs <- forM sigDs $ \(ssd@(SSD sd)) ->
  --     liftSignalDescription () ssd
  return . concat $ mfs ++ props -- ++ sigs
  where
    for = flip fmap

liftObjectPath :: ObjectPath -> ExpQ
liftObjectPath op = [| objectPath $( liftText $ objectPathToText op) |]

liftArgDesc :: ArgumentDescription n -> ExpQ
liftArgDesc Done = [|Done|]
liftArgDesc (r :> rs)  = [|$(liftText r) :> $(liftArgDesc rs)|]


toSomeMethodDescription :: Text
                        -> IInterface
                        -> IMethod
                        -> SomeMethodDescription
toSomeMethodDescription path iface imethod =
    let iInArgs = filter ((/= Just Out) . iArgumentDirection)
                         (iMethodArguments imethod)
        iOutArgs = filter ((== Just Out) . iArgumentDirection)
                         (iMethodArguments imethod)
        inArgs = toSings iInArgs
        outArgs = toSings iOutArgs
    in case (inArgs, outArgs) of
        ( SSAD (is :: Sing args) inDescs
         ,SSAD (os :: Sing rets) outDescs)
            -> withSingI is $ withSingI os $
               SMD (MD { methodObjectPath = objectPath path
                       , methodInterface = iInterfaceName iface
                       , methodMember = iMethodName imethod
                       , methodArgs = inDescs
                       , methodResult = outDescs
                       } :: MethodDescription args rets)

interfacMethodDescriptions :: Text -> IInterface -> [SomeMethodDescription]
interfacMethodDescriptions path iface =
    for (iInterfaceMethods iface) $ toSomeMethodDescription path iface
  where for = flip map

mapIInterfaces :: (Text -> IInterface -> [a]) -> Text -> INode -> [a]
mapIInterfaces f path node =
    let ifaceMembers = f path =<< nodeInterfaces node
        subNodeMembers = nodeSubnodes node >>= \n  ->
            let subPath = path <> "/" <> fromMaybe "" (nodeName n)
            in mapIInterfaces f subPath n
    in ifaceMembers ++ subNodeMembers

nodeMethodDescriptions :: Text -> INode -> [SomeMethodDescription]
nodeMethodDescriptions = mapIInterfaces interfacMethodDescriptions

interfacPropertyDescriptions :: Text -> IInterface -> [PropertyDescription]
interfacPropertyDescriptions path iface =
    for (iInterfaceProperties iface) $ \p ->
    PD { pdObjectPath = path
       , pdInterface = iInterfaceName iface
       , pdName = iPropertyName p
       , pdType = iPropertyType p
       }
  where for = flip map


-- TODO: This should be completely replaced by RemoteProperty
data PropertyDescription = PD { pdObjectPath :: Text
                              , pdInterface :: Text
                              , pdName :: Text
                              , pdType :: DBusType
                              }

nodePropertyDescriptions :: Text -> INode -> [PropertyDescription]
nodePropertyDescriptions = mapIInterfaces interfacPropertyDescriptions

liftText t = [|Text.pack $(liftString (Text.unpack  t))|]


promotedListT :: [TypeQ] -> TypeQ
promotedListT = foldr (\t ts -> appT (appT promotedConsT t) ts) promotedNilT

arrows :: [TypeQ] -> TypeQ -> TypeQ
arrows = flip $ foldr (\t ts -> appT (appT arrowT t) ts)

tupleType :: [TypeQ] -> TypeQ
tupleType xs = foldl (\ts t -> appT ts t) (tupleT (length xs)) xs

promoteSimpleType t = promotedT (mkName (show t))

promoteDBusType :: DBusType -> TypeQ
promoteDBusType (DBusSimpleType t) = [t|'DBusSimpleType $(promoteSimpleType t)|]
promoteDBusType (TypeArray t) = [t| TypeArray $(promoteDBusType t)|]
promoteDBusType (TypeStruct ts) =
    let ts' = promotedListT $ promoteDBusType <$> ts
    in [t| TypeStruct $ts'|]
promoteDBusType (TypeDict k v) =
    [t| TypeDict $(promoteSimpleType k)
                 $(promoteDBusType v) |]
promoteDBusType (TypeDictEntry k v) =
    [t| TypeDictEntry $(promoteSimpleType k)
                      $(promoteDBusType v) |]
promoteDBusType TypeVariant = [t| TypeVariant |]
promoteDBusType TypeUnit = [t| TypeUnit |]

readIntrospectXml :: FilePath -> Q INode
readIntrospectXml interfaceFile = do
    qAddDependentFile interfaceFile
    xml <- qRunIO $ BS.readFile interfaceFile
    case xmlToNode xml of
        Left e -> error $ "Could not parse introspection XML: " ++ show e
        Right r -> return r

liftMethodDescription :: String
                      -> SomeMethodDescription
                      -> Q [Dec]
liftMethodDescription name smd = case smd of
  (SMD (md :: MethodDescription args rets)) -> do
    let ats = promotedListT . map promoteDBusType $
                fromSing (sing :: Sing args)
        rts = promotedListT . map promoteDBusType $
                fromSing (sing :: Sing rets)
        md' = [|MD{ methodObjectPath = $(liftObjectPath $ methodObjectPath md)
                  , methodInterface = $(liftText $ methodInterface md)
                  , methodMember = $(liftText $ methodMember md)
                  , methodArgs = $(liftArgDesc $ methodArgs md)
                  , methodResult = $(liftArgDesc $ methodResult md)
                  } |]
    tp <- sigD (mkName name) [t|MethodDescription $(ats) $(rts)|]
    cl <- valD (varP (mkName name)) (normalB md') []
    return [tp, cl]


propertyFromDescription :: (PropertyDescription -> String)
                        -> Maybe Text
                        -> PropertyDescription
                        -> Q [Dec]
propertyFromDescription nameGen mbEntity pd = do
    entName <- newName "entity"
    let rp ent = [|RP{ rpEntity = $ent
                     , rpObject = objectPath $(liftText $ pdObjectPath pd)
                     , rpInterface = $(liftText $ pdInterface pd)
                     , rpName = $(liftText $ pdName pd)
                     } |]
        name = mkName $ nameGen pd
        entN = (mkName "entity")
        typeName = mkName "t"

        arg = case mbEntity of
            Nothing -> [[t|Text|]]
            Just _ -> []
        t = promoteDBusType $ pdType pd
    tp <- sigD name $ (arrows arg [t|RemoteProperty $(t)|])
    cl <- case mbEntity of
        Nothing -> funD name [clause [varP entN]
                              (normalB (rp (varE entN))) []]
        Just e -> valD (varP name) (normalB . rp $ liftText e) []
    return [tp, cl]

nodeSignals :: Text -> INode -> [SomeSignalDescription]
nodeSignals = mapIInterfaces interfaceSignalDs

interfaceSignalDs :: Text -> IInterface -> [SomeSignalDescription]
interfaceSignalDs ndName iface = signalDs (objectPath ndName)
                                     (iInterfaceName iface)
                                        <$> iInterfaceSignals iface

signalDs :: ObjectPath -> Text -> ISignal -> SomeSignalDescription
signalDs nPath iName iSig =
     case toSings $ iSignalArguments iSig of
      SSAD (s :: Sing ts) descs -> withSingI s $ (SSD
          (SignalDescription { signalDPath = nPath
                             , signalDInterface = iName
                             , signalDMember = iSignalName iSig
                             , signalDArguments = descs
                             } :: SignalDescription (ts :: [DBusType])
          ))

data SomeArgumentDescription where
    SSAD :: Sing (ts :: [DBusType])
         -> ArgumentDescription (ArgParity ts)
         -> SomeArgumentDescription

toSings :: [IArgument] -> SomeArgumentDescription
toSings [] = SSAD SNil Done
toSings (iarg : iargs) =
    let t = iArgumentType iarg
        desc = iArgumentName iarg
    in case (toSing t, toSings iargs) of
            (SomeSing s, SSAD ss descs)
                -> SSAD (SCons s ss) (desc :> descs)


liftSignalDescription :: String -> SomeSignalDescription -> Q [Dec]
liftSignalDescription nameString ssigDesc@(SSD (sigDesc :: SignalDescription a))
    = do
    let name = mkName nameString
        ts = fromSing (sing :: (Sing a))
        t = [t| SignalDescription $(promotedListT $ promoteDBusType <$> ts)|]
        e = [| SignalDescription
                   { signalDPath = $(liftObjectPath $ signalDPath sigDesc)
                   , signalDInterface = $(liftText $ signalDInterface sigDesc)
                   , signalDMember = $(liftText $ signalDMember sigDesc)
                   , signalDArguments = $(liftArgDesc
                                            $ signalDArguments sigDesc)
                   } |]
    tpDecl <- sigD name t
    decl <- valD (varP name) (normalB e) []
    return [tpDecl, decl]
