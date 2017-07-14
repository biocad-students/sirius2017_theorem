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
t = Var a
n = V . pack $ "n"
s = V . pack $ "s"

bool = Fa a st $ t --> t --> t
true = Lam a bool (Lam noname bool (Var a))
false = Lam noname bool (Lam b bool (Var b))
not = Lam a bool (App (App (Var a) false) true)

nat = Fa a st (t --> (t --> t) --> t)
zero = Lam x st $ Lam a (Var x) (Lam b (Var x --> Var x) t)
succ = Lam n nat $ Lam a st $ Lam z (Var a) $ Lam s (Var a --> Var a) $ Var s $$ (Var n $$ Var a $$ Var z $$ Var s)
foldNat = Lam a st $ Lam z (Var a) $ Lam s (Var a --> Var a) $ Lam n nat $ Var n $$ Var a $$ Var z $$ Var s
plus = Lam a nat $ Lam b nat $ foldNat $$ nat $$ Var a $$ Lam x nat (succ $$ Var x) $$ Var b
one = succ $$ zero
two = succ $$ one
three = succ $$ two
four = succ $$ three 

(-->) = arrow

infixr 9 `arrow`
arrow = Fa noname 

infixl 9 `app`
app = App

($$) = app