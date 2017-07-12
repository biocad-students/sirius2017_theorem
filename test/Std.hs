module Std where 

import Lawer
import Data.Text
import Prelude hiding (not)

a = V $ pack "a"
b = V $ pack "b"
x = V $ pack "x"
y = V $ pack "y"
z = V $ pack "z"
st = Uni Star
bool = Fa a (Uni Star) (Fa noname (Var a) $ Fa noname (Var a) (Var a))
true = Lam a bool (Lam noname bool (Var a))
false = Lam noname bool (Lam b bool (Var b))
not = Lam a bool (App (App (Var a) false) true)

t = Var a
nat =   Fa a (Uni Star) (t `arrow` (t `arrow` t) `arrow` t)

arrow = Fa noname 