{-# LANGUAGE RecordWildCards #-}
module Lib where

import Reduce
import Type

showTerm :: Term -> String
showTerm (Var (V n)) = n
showTerm App{..} = "(" ++ showTerm alg ++ ") (" ++ showTerm dat ++ ")"
showTerm Lam{..} = "\\" ++ showTerm (Var var) ++ " -> " ++ showTerm body

readTerm :: String -> Term
readTerm (x:xs) = undefined