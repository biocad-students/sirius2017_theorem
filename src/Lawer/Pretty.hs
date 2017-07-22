{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lawer.Pretty where 

import Lawer.Type
import Lawer.Context
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
    show App{..} = 
        case alg of
            v1@Var{..} -> case dat of
                v2@Var{} -> show v1 ++ " " ++ show v2
                _        -> show v1 ++ " (" ++ show dat ++ ")"
            (App _ _) -> case dat of
                v2@Var{} -> show alg ++ " " ++ show v2
                _        -> show alg ++ " (" ++ show dat ++ ")"
            _          -> case dat of
                v2@Var{} -> "(" ++ show alg ++ ") " ++ show v2
                _        -> "(" ++ show alg ++ ") (" ++ show dat ++ ")"

    show Lam{..} = "[" ++ show var ++ ":" ++ show tpe ++ "]" ++ show body
    show Fa{..} | var == noname = "(" ++ show tpe ++ " -> " ++ show body ++ ")"
                | otherwise = "(" ++ show var ++ ":" ++ show tpe ++ ")" ++ show body  

instance Show a => Show (Context a) where
    show Context{..} = "Context:" ++ showCtx getCtx
        where showCtx [] = []
              showCtx ((x, y):xs) = "\n\t" ++ show x ++ " : " ++ show y ++ showCtx xs