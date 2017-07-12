module Error where

import Type
import Pretty ()

data CalculusError = ParsingError String
                   | UnknownVariable Var
                   | CannotEqualize Term Term
                   | InvalidType Term String

instance Show CalculusError where
  show (ParsingError txt)      = "Parsing error:\n" ++ txt
  show (UnknownVariable v)     = "Variable '" ++ show v ++ "' is not defined"
  show (CannotEqualize t t')   = "Cannot equalize types '" ++ show t ++ "' and '" ++ show t' ++ "'"
  show (InvalidType t r)       = "Type '" ++ show t ++ "' is invalid (reason: " ++ r ++ ")"