{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module LawerHL.Pretty where

import Lawer
import LawerHL.Type

instance Show Inductive where
    show Inductive{..} = "inductive \n" ++ show indName ++ " " ++ show indParams ++ "\n" ++ show indConstructors

instance Show Record where
    show Record{..} = "record " ++ show recName ++ " " ++ show recParams ++ " = " ++ show recConstructors

instance Show Algebraic where
    show Algebraic{..} = "data " ++ show algName ++ " " ++ show algParams ++ " = " ++ show algConstructors

instance Show Construction where
    show a@Ind{} = show a
    show a@Rec{} = show a
    show a@Alg{} = show a
