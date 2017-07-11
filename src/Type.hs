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

axiom :: Uni -> Uni 
axiom Star = Box 1
axiom (Box i) = Box $ i + 1

noname :: Var 
noname = V $ pack "_"