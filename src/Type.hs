module Type where

import Data.Text (Text, pack)

type Name = Text

newtype Var = V Name
    deriving (Show, Eq, Read, Ord)

data Uni    = Star
            | Box Integer
            deriving (Show, Eq)

data Term   = Var 
            { var :: Var }
            | App 
            { alg :: Term
            , dat :: Term }
            | Lam
            { var :: Var
            , tpe :: Term
            , body :: Term } 
            | Fa 
            { var :: Var
            , tpe :: Term
            , body :: Term } 
            | Uni 
            { uni :: Uni }
            deriving (Show)

instance Eq Term where 
    (Var x) == (Var y) = x == y
    (App x y) == (App a b) = x == a && y == b
    (Uni a) == (Uni b) = a == b
    (Lam v1 t1 b1) == (Lam v2 t2 b2) = t1 == t2 && substitute b1 v1 (Var v2) == b2
    (Fa v1 t1 b1) == (Fa v2 t2 b2) = t1 == t2 && substitute b1 v1 (Var v2) == b2
    _ == _ = False

axiom :: Uni -> Uni 
axiom Star = Box 1
axiom (Box i) = Box $ i + 1

noname :: Var 
noname = V $ pack "_"