module Std where 
    
import Lawer
import LawerHL
import Data.Text
import Data.Maybe
import Prelude hiding (not, succ, and, or, lookup)

a = V $ pack "a"
b = V $ pack "b"
x = V $ pack "x"
y = V $ pack "y"
z = V $ pack "z"
st = Uni Star
bx = Uni $ Box 1
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
plus = 
        Lam a nat $ 
        Lam b nat $ 
        Var b $$ nat $$ Var a $$ succ
one = succ $$ zero
two = succ $$ one
three = succ $$ two
four = succ $$ three 

commType = Lam x nat $ Lam y nat $ eq $$ (plus $$ Var y $$ Var x) $$ (plus $$ Var x $$ Var y)
comm     = refl

-- eq =    Lam z st $ 
--         Lam a (Var z) $ 
--         Lam b (Var z) $ 
--         Fa s (Var z --> Var z --> st) $ 
--         Fa (V $ pack "refl") (Fa x (Var z) $ 
--                 Var s $$ 
--                 Var x $$ 
--                 Var x) $ 
--         Var s $$ Var a $$ Var b

-- refl =  Lam z st $ 
--         Lam y (Var z) $ 
--         Lam s (Var z --> Var z --> st) $ 
--         Lam (V $ pack "refl") 
--                 (Fa n (Var z) (Var s $$ Var n $$ Var n)) $ 
--         (Var . V $ pack "refl") $$ Var y

-- eq =    Lam a st $ 
--         Lam b st $ 
--         Fa s (st --> st --> st) $ 
--         Fa (V $ pack "refl") (Fa x st $ 
--                 Var s $$ 
--                 Var x $$ 
--                 Var x) $ 
--         Var s $$ Var a $$ Var b
    
-- refl =  Lam y st $ 
--         Lam s (st --> st --> st) $ 
--         Lam (V $ pack "refl") 
--                 (Fa n st (Var s $$ Var n $$ Var n)) $ 
--         (Var . V $ pack "refl") $$ Var y

eq = fromMaybe (Var noname) $ lookup (V $ pack "Eq") $ constructionToTerm indEq
refl = fromMaybe (Var noname) $ lookup (V $ pack "Refl") $ constructionToTerm indEq

cong = Lam a bx $ Lam b bx $ Lam x (Var a) $ Lam y (Var b) $ Lam s (Var a --> Var b) $ (eq $$ Var a $$ Var x $$ Var y) --> (eq $$ Var b $$ (Var s $$ Var x) $$ (Var s $$ Var y)) 

algBool = Alg $ Algebraic (pack "Bool") [] $ Context [(V $ pack "True", []), (V $ pack "False", [])]

algNat = Alg $ Algebraic (pack "Nat") [] $ Context [(V $ pack "Zero", []), (V $ pack "Succ", [TVar $ pack "Nat"])]

algList = Alg $ Algebraic (pack "List") [pack "a"] $ Context [(V $ pack "Nil", []), (V $ pack "Cons", [TVar $ pack "a", TApp (TVar $ pack "List") (TVar $ pack "a")])]

algPair = Alg $ Algebraic (pack "Pair") [pack "a", pack "b"] $ Context [(V $ pack "P", [TVar $ pack "a", TVar $ pack "b"])]

indEq = Ind $ Inductive (pack "Eq") 
        (Context [
                (V $ pack "A", Uni $ Box 1), 
                (V $ pack "a", Var . V $ pack "A"), 
                (V $ pack "b", Var . V $ pack "A")
                ]) $ 
        Context [
                (V $ pack "Refl", Fa x (Var . V $ pack "A") $ (Var . V $ pack "Eq") $$ (Var $ V $ pack "A") $$ Var x $$ Var x)
                ]

indList = Ind $ Inductive (pack "List") 
        (Context [(V $ pack "a", st)]) $ 
        Context [
                (V $ pack "Nil", (Var . V $ pack "List") $$ (Var . V $ pack "a")), 
                (V $ pack "Cons", aa --> ((Var . V $ pack "List") $$ aa) --> ((Var . V $ pack "List") $$ aa))
                ]

indNat = Ind $ Inductive (pack "Nat") (
        Context []
        )(
        Context [
                (V $ pack "Zero", Var . V $ pack "Nat"),
                (V $ pack "Succ", (Var . V $ pack "Nat") --> (Var . V $ pack "Nat"))
        ])