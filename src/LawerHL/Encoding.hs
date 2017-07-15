{-# LANGUAGE RecordWildCards #-}

module LawerHL.Encoding (constructionToTerm) where

import LawerHL.Type
import Lawer.Type 
import Lawer.Context 
import Lawer.Check 
import Lawer.Reduce 
import Data.Text (pack)

constructionToTerm :: Construction -> Context Term 
constructionToTerm (Ind cons) = inductiveToTerm cons
constructionToTerm (Rec cons) = recordToTerm cons
constructionToTerm (Alg cons) = algebraicToTerm cons

inductiveToTerm :: Inductive -> Context Term 
inductiveToTerm Inductive{..} = Context $ (V indName, typeWithInd indParams indConses) : getCtx indParams

recordToTerm :: Record -> Context Term 
recordToTerm = undefined

typeWithInd :: Context Term -> Context Term -> Term 
typeWithInd ctx1 ctx2 = undefined

algebraicToTerm :: Algebraic -> Context Term
algebraicToTerm Algebraic{..} = insert (V algName) (typeWith algParams algConses) $ getAllConses algConses algParams

typeWith :: [Name] -> Context [TypeApp] -> Term
typeWith params conses = foldParams params $ Fa (V $ pack "at1") (Uni Star) $ foldConses conses (Var . V $ pack "at1")

foldParams :: [Name] -> Term -> Term
foldParams arr t = foldr ((\x -> Lam x (Uni Star)) . V) t arr

foldConses :: Context [TypeApp] -> Term -> Term
foldConses (Context []) t = t
foldConses (Context ((_,x):xs)) t = fromTypeApp x `arrow` foldConses (Context xs) t

fromTypeApp :: [TypeApp] -> Term
fromTypeApp [] = Var . V $ pack "at1"
fromTypeApp (TVar x:xs) = (Var $ V x) `arrow` fromTypeApp xs
fromTypeApp (TApp x y:xs) = (fromTypeApp [x] `arrow` fromTypeApp [y]) `arrow` fromTypeApp xs

getAllConses :: Context [TypeApp] -> [Name] -> Context Term
getAllConses = undefined

infixr 9 `arrow`
arrow = Fa noname 