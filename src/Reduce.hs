{-# LANGUAGE RecordWildCards #-}
module Reduce where

import Data.Set 
import Type
import Data.Text (pack)

fresh :: Set Var -> Var
fresh conflicts = head . dropWhile (`member` conflicts) $ nameGen 
                    where nameGen = V . pack <$> [a : freshName b | b <- [0..] :: [Int], a <- ['a'..'z']]
                          freshName b = if b == 0 then "" else show b

free :: Term -> Set Var
free Var{..} = singleton var
free App{..} = free alg `union` free dat
free Lam{..} = delete var $ free body `union` free tpe
free Fa {..} = delete var $ free body `union` free tpe
free Uni{..} = empty

bound :: Term -> Set Var
bound Var{..} = empty
bound App{..} = bound alg `union` bound dat
bound Lam{..} = insert var $ bound body `union` bound tpe
bound Fa {..} = insert var $ bound body `union` bound tpe
bound Uni{..} = empty

alpha :: Term -> Set Var -> Term 
alpha term conflicts = 
    case term of 
        v@Var{} -> v
        App{..} -> App (alpha alg conflicts) (alpha dat conflicts)
        Lam{..} -> f var tpe body
        Fa {..} -> f var tpe body
        Uni{..} -> Uni uni
        where
            f var tpe body =    
                if  var `notMember` conflicts 
                then 
                    Lam var (alpha tpe conflicts) (alpha body conflicts)
                else 
                    let freshName = fresh $ free tpe `union` conflicts `union` free body 
                        subsFreshName t = substitute t freshName (Var freshName)
                    in  Lam freshName (subsFreshName tpe) (subsFreshName body)

substitute :: Term -> Var -> Term -> Term
substitute = undefined