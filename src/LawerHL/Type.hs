{-# LANGUAGE RecordWildCards #-}
module LawerHL.Type where

import Lawer
import Lawer.Type 
import Lawer.Context
import Lawer.Reduce

data Record = Record 
            { recName :: Name
            , recParams :: Context Term
            , recConses :: Context Term }


data Inductive  = Inductive 
                { indName :: Name
                , indParams :: Context Term
                , indConses :: Context Term }

data TypeApp = TVar Name 
             | TApp TypeApp TypeApp
             deriving(Show)

data Algebraic = Algebraic 
               { algName :: Name
               , algParams :: [Name]
               , algConses :: Context [TypeApp]}

data Construction   = Ind Inductive 
                    | Alg Algebraic 
                    | Rec Record
                    deriving(Show)

instance Show Inductive where
    show Inductive{..} = "inductive " ++ show indName ++ " " ++ show indParams ++ " = " ++ show indConses

instance Show Record where
    show Record{..} = "record " ++ show recName ++ " " ++ show recParams ++ " = " ++ show recConses

instance Show Algebraic where
    show Algebraic{..} = "data " ++ show algName ++ " " ++ show algParams ++ " = " ++ show algConses
