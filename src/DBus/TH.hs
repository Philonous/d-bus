{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module DBus.TH where

import Control.Applicative ((<$>), (<*>))
import Control.Monad
import DBus.Types
import Data.Singletons (SingI)
import Language.Haskell.TH

for :: Functor f => f a -> (a -> b) -> f b
for = flip fmap

litStruct :: [ExpQ] -> ExpQ
litStruct xs = foldr cons (sing $ last xs) $ init xs
  where
    sing nm = (appE (conE 'StructSingleton) nm)
    cons nm xs' = (appE (appE (conE 'StructCons) nm) xs')

litStructPat :: [PatQ] -> PatQ
litStructPat xs = foldr cons (sing $ last xs) $ init xs
  where
    sing ps = conP 'StructSingleton [ps]
    cons p ps = conP 'StructCons [p, ps]

caseMaybes :: [(Name, Name)] -> ExpQ -> ExpQ
caseMaybes [] e = e
caseMaybes ((tmp,x):xs) e =
   caseE (appE (varE 'fromRep) (varE tmp))
         [ match (conP 'Nothing []) (normalB $ conE 'Nothing) []
         , match (conP 'Just [varP x]) (normalB $ caseMaybes xs e) []
         ]

fromTyVarBndr :: TyVarBndr -> Type
fromTyVarBndr (PlainTV n) = VarT n
fromTyVarBndr (KindedTV n _) = VarT n

fromConstr :: Con -> (Name, [Type])
fromConstr (NormalC n stps) = (n, map snd stps)
fromConstr (RecC n vstps)   = (n, map (\(_,_,t) -> t) vstps)
fromConstr _ = error "fromConstr only handles NormalC and RecC"

tyVarName :: TyVarBndr -> Name
tyVarName (PlainTV n) = n
tyVarName (KindedTV n _k) = n

promotedListT :: [TypeQ] -> TypeQ
promotedListT xs = foldr appT promotedNilT (map (appT promotedConsT)xs)

relevantTyVars :: [Con] -> [Name]
relevantTyVars constrs = concatMap tyVars constrs
  where
    tyVars constr = concatMap tyVar . snd $ fromConstr constr
    tyVar (VarT n) = [n]
    tyVar _ = []

-- | Create a 'Representable' instance for a type.
--
-- The strategy used to marshal types depends on the shape of your type:
--
-- * If none of the constructor(s) have fields, the type is represented by a
--   @Byte@ where the n-th constructor (counting from 0) is marshalled to n
--
-- * If the type has a single constructor with exactly one field, is is
--   represented by the 'RepType' of it's field. (This is always the case for
--   @newtypes@)
--
-- * If the type has a single constructor with multiple fields, it is
--   represented by a @Struct@ (consisting of the translated members)
--
-- * In the general case with multiple constructors with varying numbers of
--   fields , the type is represented by a pair (2-element struct) of a @Byte@
--   (tag) and a @Variant@. The n-th constructor (counting from 0) is
--   represented by the tag n and contents of the @Variant@ depends on the
--   number of members:
--
-- * For constructors without members, a single @Byte@ with the value 0 is
--   stored (The value of the Byte is ignored when unmarshalling)
--
-- * For constructors with a single member, the translated member is stored as-is
--
-- * For constructors with multiple members, the translated members are stored in a
--  @Struct@
makeRepresentable :: Name -> Q [Dec]
makeRepresentable name = do
    TyConI t <- reify name
    let (_numTyParams, tyVarNames, cons) = case t of
#if MIN_VERSION_template_haskell(2,11,0)
            NewtypeD _ _ tvs _ c _ -> (length tvs, tyVarName <$> tvs, [c])
            DataD _ _ tvs _ cs' _ -> (length tvs, tyVarName <$> tvs, cs')
#else
            NewtypeD _ _ tvs c _ -> (length tvs, tyVarName <$> tvs, [c])
            DataD _ _ tvs cs _ -> (length tvs, tyVarName <$> tvs, cs)
#endif
            _ -> error "makeReprsentable only handles Data and Newtype declarations"
        ctx1 = mapM (appT (conT ''SingI) . appT (conT ''RepType)) (varT <$> (relevantTyVars cons))
        ctx2 = mapM (appT (conT  ''Representable)) (varT <$> relevantTyVars cons)
        ctx = liftM2 (++) ctx1 ctx2
        fullType = (foldl appT (conT name) (varT <$> tyVarNames))
        iHead = appT (conT ''Representable) fullType
        cs = map fromConstr cons
    (repType, toClauses, fromClauses) <-  case all (null . snd) cs of
        True -> enumerate $ map fst cs
        False -> case map fromConstr cons of
            [] -> fail "Can't make representation of empty data type"
            cs' | (all (null . snd) cs) -> enumerate $ map fst cs'
            [(conName, fields)] -> oneCon conName fields
            cs' -> multiCon cs'
    inst <- instanceD ctx iHead
        [ tySynInstD ''RepType $ tySynEqn [fullType] repType
        , funD 'toRep toClauses
        , funD 'fromRep fromClauses
        ]
    return [inst]
  where

    oneCon conName fieldTypes = do
        (repType
          , (toPat, toBD)
          , (fromPat, fromBD)) <- singleConstructor conName fieldTypes
        return ( repType
               , [clause [toPat] (normalB toBD) []]
               , [clause [fromPat] (normalB fromBD) []]
               )
    enumerate conNames
        = return ( [t| 'DBusSimpleType 'TypeByte |]
                 , for (zip conNames [0..]) $ \(cn, i) ->
                       (clause [conP cn []] (normalB . appE (conE 'DBVByte)
                                                     . litE $ integerL i) [])
                 , (for (zip conNames [0..]) $ \(cn, i) ->
                       (clause [(conP 'DBVByte) . (:[]) . litP $ integerL i] (normalB $ appE (conE 'Just)
                                                                   (conE cn)) []))
                   ++ [clause [wildP] (normalB $ conE 'Nothing) []]
                 )
    multiCon :: [(Name, [Type])] -> Q (TypeQ, [ClauseQ], [ClauseQ])
    multiCon cs = do
        clauses <- forM (zip [0..] cs) $ \(branch, (conName, ts)) -> do
             ( repType
               , (toPat, toBD)
               , (fromPat', fromBD')) <- singleConstructor conName ts
             let bd = appE (conE 'DBVStruct) $
                            litStruct [ (appE (conE 'DBVByte) .
                                              litE $ integerL branch)
                                      , appE (conE 'DBVVariant) toBD]
             varName <- newName "x"
             let fromPat = conP 'DBVStruct [
                             litStructPat [ conP 'DBVByte [litP $ integerL branch]
                                          , varP varName
                                          ]]
                 fromBD = caseE [|fromVariant $(varE varName) :: Maybe (DBusValue $repType) |]
                             [ match (conP 'Nothing [])
                                     (normalB $ conE 'Nothing) []
                             , match (conP 'Just [fromPat'])
                                     (normalB $ fromBD') []
                             ]

             return $ ( clause [toPat] (normalB bd) []
                      , clause [fromPat] (normalB fromBD) []
                      )

        return ( [t| 'TypeStruct '[ 'DBusSimpleType 'TypeByte, 'TypeVariant] |]
               , map fst clauses
               , map snd clauses
               )
    singleConstructor conName [] = do
        return $ ( [t| 'DBusSimpleType 'TypeByte |]
                 , ( conP conName []
                   , [| DBVByte 0|]
                   )
                 , ( [p| DBVByte _ |]
                   , [| Just $(conE conName)|]
                   )
                 )
    singleConstructor conName [t] = do
        var <- newName "x"
        return $ ( [t| RepType $(return t) |]
                 , ( conP conName [varP var]
                   , [| toRep $(varE var)|]
                   )
                 , ( varP var
                   , [| $(conE conName) <$>fromRep $(varE var)|]
                   )
                 )

    singleConstructor conName ts = do
        (varNames, tmpNames) <- unzip <$> (forM ts $ \_ -> (,) <$> (newName "x")
                                                              <*> (newName "mbx"))

        return ( appT (promotedT 'TypeStruct) . promotedListT $
                             (map (appT (conT ''RepType) . return) ts)
               , let
                 pats = conP conName (map varP varNames)
                 bs = (appE (conE 'DBVStruct)
                               (litStruct $ map (appE (varE 'toRep) . varE) varNames))
                 in (pats, bs)
               , let
                 pats = conP 'DBVStruct [litStructPat $ varP <$> tmpNames]
                 bs = (caseMaybes (zip tmpNames varNames)
                                    (appE (conE 'Just)
                                $ foldl appE (conE conName) (varE <$> varNames)))
                  in (pats, bs)
               )

-- TODO: Fold into makeRepresentable

-- | Create a 'Representable' instance for a Tuple. The tuple will be
-- represented by a @Struct@. Instances for Tuples up to length 20 are already
-- provided
makeRepresentableTuple :: Int -> Q Dec
makeRepresentableTuple num = do
    let names = take num $ map (varT . mkName . (:[])) ['a' .. 'z']
        ctx = sequence $ appT (conT  ''Representable) <$> names
        tp = (foldl appT (tupleT num) names)
        iHead = appT (conT ''Representable)
                     (foldl appT (tupleT num) names)
        tpList = foldr (appT . appT promotedConsT) promotedNilT
                 (appT (conT ''RepType) <$> names)
    varNames <- replicateM num (newName "x")
    tmpNames <- replicateM num (newName "mbx")
    instanceD ctx iHead
        [ tySynInstD ''RepType $ tySynEqn [tp]
                      (appT (promotedT 'TypeStruct) tpList)
        , funD ('toRep) $ let
               pats = [tupP (map varP varNames)]
               bs = normalB (appE (conE 'DBVStruct)
                             (litStruct $ map (appE (varE 'toRep) . varE) varNames))
               in [clause pats bs []]
        , funD ('fromRep) $ let
               pats = [conP 'DBVStruct [litStructPat $ varP <$> tmpNames]]
               bs = normalB (caseMaybes (zip tmpNames varNames)
                            (appE (conE 'Just)
                            $ tupE (varE <$> varNames)))
               in [clause pats bs []]
        ]
