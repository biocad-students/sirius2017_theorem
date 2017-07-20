module Lawer.Context where

import           Lawer.Type
import           Prelude            hiding (lookup)
import qualified Prelude              as P (lookup)

newtype Context a = Context
                { getCtx :: [(Var, a)] }
                deriving (Eq)

lookup :: Var -> Context a -> Maybe a 
lookup v (Context arr) = P.lookup v arr

empty :: Context a
empty = Context []

insert :: Var -> a -> Context a -> Context a
insert v t = Context . ((v, t):) . getCtx