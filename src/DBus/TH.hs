{-# LANGUAGE TemplateHaskell #-}

module DBus.TH where

import Control.Applicative ((<$>))
import DBus.Types
import Control.Monad

import Language.Haskell.TH

-- makeRepresentable name = do
--     TyConI t <- reify
--     (tvs, con) <- case t of
--         DataD _ _ tvs [c] -> return (tvs, c)
--         NewTypeD _ _ tvs c -> return (tvs, c)
--     InstanceD [] (ConT name)
--     {[|

makeRepresentableTuple num = do
    let names = take num $ map (VarT . mkName . (:[])) ['a' .. 'z']
        ctx = ClassP ''DBusRepresentable . (:[]) <$> names
        tp = (foldl AppT (TupleT num) names)
        iHead = AppT (ConT ''DBusRepresentable)
                     (foldl AppT (TupleT num) names)
        tpList = foldr (AppT . AppT PromotedConsT) PromotedNilT
                 (AppT (ConT ''RepType) <$> names)
        repTp = AppT (PromotedT 'TypeStruct) tpList
    varNames <- replicateM num (newName "x")
    tmpNames <- replicateM num (newName "mbx")
    return $ InstanceD ctx iHead
        [ TySynInstD ''RepType [tp] (AppT (PromotedT 'TypeStruct) tpList)
        , FunD ('toRep) $ let
               pats = [TupP (map VarP varNames)]
               bs = NormalB (AppE (ConE 'DBVStruct)
                             (litStruct $ map (AppE (VarE 'toRep) . VarE) varNames))
               in [Clause pats bs []]
        , FunD ('fromRep) $ let
               pats = [ConP 'DBVStruct [litStructPat tmpNames]]
               bs = NormalB (caseMaybes (zip tmpNames varNames)
                            (AppE (ConE 'Just)
                            $ TupE (VarE <$> varNames)))
               in [Clause pats bs []]
        ]
  where
    litStruct xs = foldr cons (sing $ last xs) $ init xs
      where
        sing nm = (AppE (ConE 'StructSingleton) nm)
        cons nm xs = (AppE (AppE (ConE 'StructCons) nm) xs)
    litStructPat xs = foldr cons (sing $ last xs) $ init xs
      where
        sing nm = ConP 'StructSingleton [VarP nm]
        cons nm xs = ConP 'StructCons [VarP nm, xs]
    caseMaybes [] e = e
    caseMaybes ((tmp,x):xs) e =
       CaseE (AppE (VarE 'fromRep) (VarE tmp))
             [ Match (ConP 'Nothing []) (NormalB $ ConE 'Nothing) []
             , Match (ConP 'Just [VarP x]) (NormalB $ caseMaybes xs e) []
             ]
