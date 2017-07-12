module Lawer.Context where

import           Lawer.Type
import           Prelude            hiding (lookup)
import qualified Prelude                    as P (lookup)

newtype Context = Context 
                { getCtx :: [(Var, Term)] }

lookup :: Var -> Context -> Maybe Term 
lookup v (Context arr) = P.lookup v arr

empty :: Context
empty = Context []

insert :: Var -> Term -> Context -> Context
insert v t = Context . ((v, t):) . getCtx