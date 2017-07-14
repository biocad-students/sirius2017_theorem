module LawerHL.Type where

import Lawer.Type 
import Lawer.Context
import Lawer.Reduce

data Record = Record 
            { recName :: Name
            , recParams :: Context Term
            , recConses :: Context Term }
            deriving(Show)


data Inductive  = Inductive 
                { indName :: Name
                , indParams :: Context Term
                , indConses :: Context Term }
                deriving(Show)

data TypeApp = TVar Name 
             | TApp TypeApp TypeApp
             deriving(Show)

data Algebraic = Algebraic 
               { algName :: Name
               , algParams :: [Name]
               , algConses :: Context [TypeApp]}
               deriving(Show)

data Construction   = Ind Inductive 
                    | Alg Algebraic 
                    | Rec Record
                    deriving(Show)