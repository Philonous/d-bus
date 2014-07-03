{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

-- | TH helpers to build scaffolding from introspection data


module DBus.Scaffold where

import           Control.Applicative
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as Text
import           Language.Haskell.TH
import           Language.Haskell.TH.Lib
import           Language.Haskell.TH.Syntax

import           DBus.Introspect
import           DBus.Message
import           DBus.Representable
import           DBus.Types


data MethodDescription = MD { methodObjectPath :: Text
                            , methodInterface :: Text
                            , methodMember :: Text
                            , methodArgTypes :: [DBusType]
                            , methodReturnTypes :: [DBusType]
                            } deriving (Eq, Show)

interfacMethodDescriptions path iface =
    for (iInterfaceMethods iface) $ \m ->
    MD { methodObjectPath = path
       , methodInterface = iInterfaceName iface
       , methodMember = iMethodName m
       , methodArgTypes = iArgumentType
                                <$> filter ((/= Just Out) . iArgumentDirection)
                                (iMethodArguments m)
       , methodReturnTypes = iArgumentType
                             <$> filter ((== Just Out) . iArgumentDirection)
                             (iMethodArguments m)
       }
  where for = flip map

nodeMethodDescriptions path node =
    let ifaceMembers = interfacMethodDescriptions path =<< nodeInterfaces node
        subNodeMembers = nodeSubnodes node >>= \n  ->
            let subPath = path <> "/" <> nodeName n
            in nodeMethodDescriptions subPath n
    in ifaceMembers ++ subNodeMembers

liftText t = [|Text.pack $(liftString (Text.unpack  t))|]


promotedListT = foldr (\t ts -> appT (appT promotedConsT t) ts) promotedNilT

arrows :: [TypeQ] -> TypeQ -> TypeQ
arrows = flip $ foldr (\t ts -> appT (appT arrowT t) ts)

tupleType :: [TypeQ] -> TypeQ
tupleType xs = foldl (\ts t -> appT ts t) (tupleT (length xs)) xs

promoteDBusType :: DBusType -> TypeQ
promoteDBusType (DBusSimpleType t) = [t|'DBusSimpleType
                                        $(promotedT (mkName (show t)))|]
promoteDBusType (TypeArray t) = [t| TypeArray $(promoteDBusType t)|]
promoteDBusType (TypeStruct ts) =
    let ts' = promotedListT $ promoteDBusType <$> ts
    in [t| TypeStruct $ts'|]
promoteDBusType (TypeDict k v) =
    [t| TypeDict $(promoteDBusType (DBusSimpleType k))
                 $(promoteDBusType v) |]
promoteDBusType (TypeDictEntry k v) =
    [t| TypeDictEntry $(promoteDBusType (DBusSimpleType k))
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


methodFunction :: (MethodDescription -> String) -- ^ Generate names from Method
                                                -- descriptions
               -> Maybe Text -- ^ Just name to fix the entity, Nothing to leave
                             -- it as a parameter
               -> MethodDescription -- ^ The method description to generate a
                                    -- function from
               -> Q [Dec]
methodFunction nameGen mbEntity method = do
    let name = mkName (nameGen method)
    conName <- newName "con"
    argNames <- forM (methodArgTypes method) $ \_ -> newName "x"
    argTypeNames <- forM (methodArgTypes method) $ \_ -> newName "t"
    resTypeName <- newName "r"
    let tyVarBndrs = plainTV <$> (argTypeNames ++ [resTypeName])
        representables = map (\t -> classP ''Representable [varT t])
                             (argTypeNames ++ [resTypeName])
        repTypes = zipWith (\n t -> equalP [t|RepType $(varT n)|] t)
                       (argTypeNames)
                       (promoteDBusType <$> methodArgTypes method)

        resType = case methodReturnTypes method of
            [] -> [t|TypeUnit|]
            [t] -> promoteDBusType t
            _ -> let resTypes = promotedListT
                                  (promoteDBusType <$> methodReturnTypes method)
                 in [t|TypeStruct $resTypes|]
        resConstr = (equalP [t|RepType $(varT resTypeName)|]
                            resType)
        context = representables ++ repTypes ++ [resConstr]
        entityType = case mbEntity of
            Nothing -> [[t|Text|]]
            Just _ -> []
    entityName <- newName "entity"
    let entityVar = case mbEntity of
            Nothing -> [varP entityName]
            Just _ -> []
        entityE = case mbEntity of
            Just e -> liftText e
            Nothing -> varE entityName
        argsE = case argNames of
                  [n] -> varE n
                  _ -> tupE (varE <$> argNames)
    tp <- sigD name (forallT tyVarBndrs (sequence context)
                     (arrows ( entityType
                             ++ (varT <$> argTypeNames)
                             ++ [[t| DBusConnection|]]
                             )
                             [t| IO (Either MethodError $(varT resTypeName))|]))
    fun <- funD name
        [clause ( entityVar ++ map varP argNames ++ [varP conName])
            (normalB [|callMethod
                         $(entityE)
                         (objectPath $(liftText $ methodObjectPath method))
                         $(liftText $ methodInterface method)
                         $(liftText $ methodMember method)
                         $(argsE)
                         []
                         $(varE conName)
                      |])
          []
         ]
    return [tp, fun]
