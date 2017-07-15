module LawerHL.Type where

import Lawer.Type 
import Lawer.Context
import Lawer.Reduce
import Lawer.Pretty

data Record = Record 
            { recName :: Name
            , recParams :: Context Term
            , recConses :: Context Term }


data Inductive  = Inductive 
                { indName :: Name
                , indParams :: Context Term
                , indConses :: Context Term }

data TypeApp = TVar Name 
             | TApp TypeApp TypeApp
             deriving(Show)

data Algebraic = Algebraic 
               { algName :: Name
               , algParams :: [Name]
               , algConses :: Context [TypeApp]}

data Construction   = Ind Inductive 
                    | Alg Algebraic 
                    | Rec Record