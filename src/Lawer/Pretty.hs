{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lawer.Pretty where 

import Lawer.Type
import Data.Text (unpack)

instance Show Var where
    show (V name) = unpack name

instance Show Uni where 
    show Star = "*"
    show (Box 1) = "[]"
    show (Box i) = "[" ++ show i ++ "]" 

instance Show Term where
    show Uni{..} = show uni
    show Var{..} = show var
    show App{..} = show alg ++ "(" ++ show dat ++ ")"
    show Lam{..} = "[" ++ show var ++ ":" ++ show tpe ++ "]" ++ show body
    show Fa{..} = "(" ++ show var ++ ":" ++ show tpe ++ ")" ++ show body
    