{-# LANGUAGE RecordWildCards #-}
module Reduce where

import Type 

reduce :: Term -> Term
reduce a@(Var _) = a
reduce (App (Lam a b) c) = subs b a c
reduce a = id a

subs :: Term -> Var -> Term -> Term
subs (Var a) x term | a == x    = term
                    | otherwise = Var a
subs (App a b) x term = App (subs a x term) (subs b x term)
subs (Lam a b) x term = Lam a (subs b x term)