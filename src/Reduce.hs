module Reduce where

import Type 
import Data.List

free :: Term -> [Var]
free (Var x) = [x]
free (App a b) = nub $ free a ++ free b
free (Lam a b) = delete a $ free b

binded :: Term -> [Var]
binded (Var _) = []
binded (App a b) = nub $ binded a ++ binded b
binded (Lam a b) = nub $ a : binded b

fresh :: [Var] -> Var
fresh arr = head $ filter (`notElem` arr) a
            where a = [V $ x : show i | i <- [1..], x <- ['a'..'z']]

alpha :: Term -> [Var] -> Term 
alpha x@(Var _) _ = x
alpha (App a b) arr = App (alpha a arr) (alpha b arr)
alpha (Lam a b) arr | a `elem` arr =    let x = fresh (arr ++ free b)
                                        in Lam x $ subs b a (Var x)
                    | otherwise    = Lam a (alpha b arr)

subs :: Term -> Var -> Term -> Term 
subs (Var x) v t    | x == v    = t
                    | otherwise = Var x
subs (App a b) v t              = App (subs a v t) (subs b v t)
subs (Lam a b) v t  | a == v    = Lam a b
                    | otherwise = let lam = alpha (Lam a b) (free t)
                                  in Lam (var lam) (subs (body lam) v t)

beta :: Term -> Term 
beta (Var x) = Var x
beta (App (Lam a b) t) = subs b a t
beta (App a b) = let a' = beta a
                     b' = beta b
                 in if a /= a' then App a' b else App a b'
beta (Lam a b) = Lam a (beta b)

eta :: Term -> Term 
eta (Var x) = Var x
eta (App a b) = App (eta a) (eta b)
eta (Lam a (App b x))   | Var a == x = b
                        | otherwise = Lam a (eta $ App b x)
eta (Lam a b) = Lam a (eta b)

reduce :: Term -> Term 
reduce a | a' == a && a == reduce' a = a
         | a' == a = reduce . reduce' $ a
         | otherwise = reduce . eta $ a'
             where a' = eta a

reduce' :: Term -> Term 
reduce' a = let a' = beta a
            in  if a' == a 
                then a
                else reduce' a' 