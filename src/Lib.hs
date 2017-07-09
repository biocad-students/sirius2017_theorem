{-# LANGUAGE RecordWildCards #-}
module Lib where

import Type
import Reduce
import Substitution

showTerm :: Term -> String
showTerm (Var (V n)) = n
showTerm App{..} = "(" ++ showTerm alg ++ ") (" ++ showTerm dat ++ ")"
showTerm Lam{..} = "\\" ++ showTerm (Var var) ++ " -> " ++ showTerm body

readTerm :: String -> Term
readTerm = undefined