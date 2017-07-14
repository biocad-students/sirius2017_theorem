{-# LANGUAGE RecordWildCards #-}
module LawerHL.Pretty where

import Lawer
import LawerHL.Type

instance Show Inductive where
    show Inductive{..} = "inductive " ++ show indName ++ " " ++ show indParams ++ " = " ++ show indConss