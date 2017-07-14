{-# LANGUAGE RecordWildCards #-}
module LawerHL.Pretty where

import Lawer
import LawerHL.Type

instance Show Inductive where
    show Inductive{..} = "inductive " ++ show indName ++ " " ++ show indParams ++ " = " ++ show indConss
	
instance Show Record where
    show Record{..} = "record " ++ show recName ++ " " ++ show recParams ++ " = " ++ show recConss