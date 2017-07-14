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
n = V . pack $ "n"
s = V . pack $ "s"
nat = Fa a st (t `arrow` (t `arrow` t) `arrow` t)
zero = Lam x st $ Lam a (Var x) (Lam b (Var x `arrow` Var x) t)
succ = Lam n nat $ Lam a st $ Lam z (Var a) $ Lam s (Var a `arrow` Var a) $ Var s $$ (Var n $$ Var a $$ Var z $$ Var s)

-- \(n : Nat) -> \(a : *) -> \(z : a) -> \(s : a -> a) -> s (n a z s)

foldNat = Lam a st $ Lam z (Var a) $ Lam s (Var a `arrow` Var a) $ Lam n nat $ Var n `app` Var a `app` Var z `app` Var s

-- foldNat = \(a : *) -> \(z : a) -> \(s : a -> a) -> \(n : Nat) -> (n a z s)

plus = Lam a nat $ Lam b nat $ foldNat `app` nat `app` Var a `app` Lam x nat (succ `app` Var x) `app` Var b

-- plus = \(n : Nat) -> \(m : Nat) -> foldNat Nat n (\(x : Nat) -> S x) m
one = succ `app` zero
two = succ `app` one
three = succ `app` two
four = succ `app` three

infixr 9 `arrow`
arrow = Fa noname 

infixl 9 `app`
app = App

infixl 9 $$
($$) = app