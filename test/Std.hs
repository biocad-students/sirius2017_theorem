module Std where 
    
import Lawer
import LawerHL
import Data.Text
import Prelude hiding (not, succ, and, or)

a = V $ pack "a"
b = V $ pack "b"
x = V $ pack "x"
y = V $ pack "y"
z = V $ pack "z"
st = Uni Star
aa = Var a
n = V . pack $ "n"
s = V . pack $ "s"

bool = Fa a st $ aa --> aa --> aa
true = Lam a st $ Lam (V $ pack "True") aa $ Lam noname aa (Var . V $ pack "True") 
false = Lam a st $ Lam noname aa $ Lam (V $ pack "False") aa (Var . V $ pack "False")
not = Lam a bool $ aa $$ bool $$ false $$ true
and = Lam a bool $ Lam b bool $ Var a $$ bool $$ Var b $$ false
or = Lam a bool $ Lam b bool $ Var a $$ bool $$ true $$ Var b

nat = Fa a st (aa --> (aa --> aa) --> aa)
zero = Lam x st $ Lam a (Var x) (Lam noname (Var x --> Var x) aa)
succ = Lam n nat $ Lam a st $ Lam z (Var a) $ Lam s (Var a --> Var a) $ Var s $$ (Var n $$ Var a $$ Var z $$ Var s)
foldNat = Lam a st $ Lam z (Var a) $ Lam s (Var a --> Var a) $ Lam n nat $ Var n $$ Var a $$ Var z $$ Var s
plus = Lam a nat $ Lam b nat $ foldNat $$ nat $$ Var a $$ Lam x nat (succ $$ Var x) $$ Var b
one = succ $$ zero
two = succ $$ one
three = succ $$ two
four = succ $$ three 

commType = Lam x nat $ Lam y nat $ eq $$ (plus $$ Var y $$ Var x) $$ (plus $$ Var x $$ Var y)
comm     = refl

eq =    Lam a st $ 
        Lam b st $ 
        Fa n (st --> st --> st) $
        Var n $$ Var a $$ Var b
refl =  Fa x st $ eq $$ Var x $$ Var x
    

algBool = Alg $ Algebraic (pack "Bool") [] $ Context [(V $ pack "True", []), (V $ pack "False", [])]

algNat = Alg $ Algebraic (pack "Nat") [] $ Context [(V $ pack "Zero", []), (V $ pack "Succ", [TVar $ pack "Nat"])]

algList = Alg $ Algebraic (pack "List") [pack "a"] $ Context [(V $ pack "Nil", []), (V $ pack "Cons", [TVar $ pack "a", TApp (TVar $ pack "list") (TVar $ pack "a")])]

algPair = Alg $ Algebraic (pack "Pair") [pack "a", pack "b"] $ Context [(V $ pack "P", [TVar $ pack "a", TVar $ pack "b"])]