module Lawer(
    axiom, noname, Name, Var(..), Uni(..), Term(..),
    typeOf, 
    reduce, substitute,
    Context(..), lookup, empty, insert,
    CalculusError(..)
) where

import Lawer.Type (axiom, noname, Name, Var(..), Uni(..), Term(..))
import Lawer.Reduce (reduce, substitute)
import Lawer.Pretty ()
import Lawer.Error (CalculusError(..))
import Lawer.Check (typeOf)
import Lawer.Context (Context(..), lookup, empty, insert)
import Prelude hiding (lookup)