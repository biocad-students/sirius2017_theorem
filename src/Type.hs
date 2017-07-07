module Type where

type Name = String

newtype Var = V Name
    deriving (Show, Read, Eq)

data Term   = Var { var     :: Var  } 
            | App { alg     :: Term 
                  , dat     :: Term }
            | Lam { var     :: Var  
                  , body    :: Term }
    deriving (Show, Read)