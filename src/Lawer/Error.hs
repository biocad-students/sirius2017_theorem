module Lawer.Error where

import Lawer.Type
import Lawer.Pretty ()
import Lawer.Reduce ()

data CalculusError = ParsingError String
                   | UnknownVariable Var
                   | CannotEqualizeTypes Term Term
                   | InvalidType Term String
                   deriving(Eq)

instance Show CalculusError where
  show (ParsingError txt)         = "Parsing error:\n" ++ txt
  show (UnknownVariable v)        = "Variable '" ++ show v ++ "' is not defined"
  show (InvalidType t r)          = "Type '" ++ show t ++ "' is invalid (reason: " ++ r ++ ")"
  show (CannotEqualizeTypes t t') = "Types: '" ++ show t ++ "' != '" ++ show t' ++ "'"