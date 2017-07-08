module Reduce where

import Type 

reduce :: Term -> Term
reduce = reduce'' . renameAll

reduce'' :: Term -> Term
reduce'' t =    if reduce' t == t 
                then t 
                else reduce'' . reduce' $ t

reduce' :: Term -> Term
reduce' a@(Var _) = a
reduce' (App (Lam a b) c) = subs b a c
reduce' (App a b) = App (reduce' a) (reduce' b)
reduce' (Lam a b) = Lam a $ reduce' b
reduce' a = id a

subs :: Term -> Var -> Term -> Term
subs (Var a) x term | a == x    = term
                    | otherwise = Var a
subs (App a b) x term = App (subs a x term) (subs b x term)
subs (Lam a b) x term = Lam a (subs b x term)

renameAll :: Term -> Term
renameAll = rename' [] 0 

rename' :: [(Var, Int)] -> Int -> Term -> Term
rename' arr i (Lam (V name) b) = Lam (V . (++ show i) $ name) (rename' ((V name, i):arr) (i + 1) b)
rename' arr i (Var (V name)) = case lookup (V name) arr of
                            Just i -> Var . V $ name ++ show i
                            Nothing -> Var . V $ name
rename' arr i term = let terms = termToTerms term
                     in  foldl (\a b -> App a $ rename' arr i b) (Lam (V $ "w99") (Var . V $ "w99")) terms

termToTerms :: Term -> [Term]
termToTerms (App a b) = termToTerms a ++ [b]
termToTerms a = pure a