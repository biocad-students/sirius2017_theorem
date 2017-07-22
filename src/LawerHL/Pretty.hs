{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module LawerHL.Pretty where

import Lawer
import LawerHL.Type

--instance Show Inductive where
--    show Inductive{..} = "inductive " ++ show indName ++ " " ++ show indParams ++ " = " ++ show indConses
--
--instance Show Record where
--    show Record{..} = "record " ++ show recName ++ " " ++ show recParams ++ " = " ++ show recConses
--
--instance Show Algebraic where
--    show Algebraic{..} = "data " ++ show algName ++ " " ++ show algParams ++ " = " ++ show algConses
