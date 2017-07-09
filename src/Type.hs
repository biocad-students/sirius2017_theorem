{-# LANGUAGE RecordWildCards #-}
module Type where

type Name = String

newtype Var = V Name
    deriving (Show, Read, Eq)

data Term   = Var { var     :: Var  } 
            | App { alg     :: Term 
                  , dat     :: Term }
            | Lam { var     :: Var  
                  , body    :: Term }
    deriving (Read, Eq)

instance Show Term where 
    show (Var (V n)) = n
    show App{..} = "(" ++ show alg ++ ") (" ++ show dat ++ ")"
    show Lam{..} = "\\" ++ show (Var var) ++ " -> " ++ show body

data Type =   Type Var
            | Arrow Type Type
            deriving (Read, Eq)
instance Show Type where
    show (Type (V n)) = n
    show (Arrow a b) = "(" ++ show a ++ ") -> " ++ show b

newtype Context = Context [(Var, Type)]
            deriving (Show, Read, Eq)

data TS = TS Var Type
            deriving (Show, Read, Eq)

type Substitution = [TS]
