module Std where 

import Lawer
import Data.Text
import Prelude hiding (not, succ)

a = V $ pack "a"
b = V $ pack "b"
x = V $ pack "x"
y = V $ pack "y"
z = V $ pack "z"
st = Uni Star

bool = Fa a st $ t `arrow` t `arrow` t
true = Lam a bool (Lam noname bool (Var a))
false = Lam noname bool (Lam b bool (Var b))
not = Lam a bool (App (App (Var a) false) true)

t = Var a
nat = Fa a st (t `arrow` (t `arrow` t) `arrow` t)
zero = Lam x st $ 
    Lam a (Var x) (Lam b (Var x `arrow` Var x) t)
succ = Lam a nat $ Lam x (nat `arrow` nat) $ Lam z nat $ App (Var x) $ App (App (Var x) (Var a)) $ Var z

infixr 9 `arrow`
arrow = Fa noname 