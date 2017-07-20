{-# LANGUAGE RecordWildCards #-}
module LawerHL.Type where

import Lawer
import Lawer.Type 
import Lawer.Context
import Lawer.Reduce
import Lawer.Pretty ()

data Record = Record 
            { recName :: Name
            , recParams :: Context Term
            , recConstructors :: Context Term }
            deriving(Eq)


data Inductive  = Inductive 
                { indName :: Name
                , indParams :: Context Term
                , indConstructors :: Context Term }
                deriving(Eq)

data TypeApp = TVar Name 
             | TApp TypeApp TypeApp
             deriving(Show, Eq)

data Algebraic = Algebraic 
               { algName :: Name
               , algParams :: [Name]
               , algConstructors :: Context [TypeApp]}
               deriving(Eq)

data Construction   = Ind Inductive 
                    | Alg Algebraic 
                    | Rec Record
                    deriving(Show, Eq)

instance Show Inductive where
    show Inductive{..} = "inductive " ++ show indName ++ " " ++ show indParams ++ " = " ++ show indConstructors

instance Show Record where
    show Record{..} = "record " ++ show recName ++ " " ++ show recParams ++ " = " ++ show recConstructors

instance Show Algebraic where
    show Algebraic{..} = "data " ++ show algName ++ " " ++ show algParams ++ " = " ++ show algConstructors

