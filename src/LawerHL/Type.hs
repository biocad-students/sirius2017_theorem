module LawerHL.Type where

import Lawer.Type 
import Lawer.Context
import Lawer.Reduce

data Record = Record 
               { recName :: Name
               , recParams :: Context Term
               , recConss :: Context Term }


data Inductive = Inductive 
                { indName :: Name
                , indParams :: Context Term
                , indConss :: Context Term }