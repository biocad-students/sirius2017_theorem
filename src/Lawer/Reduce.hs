{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lawer.Reduce (Term (..), reduce, substitute) where

import Data.Set (singleton, union, empty, delete, insert, member, notMember, Set) 
import Lawer.Type
import Data.Text (pack)

instance Eq Term where 
    (Var x) == (Var y) = x == y
    (App x y) == (App a b) = x == a && y == b
    (Uni a) == (Uni b) = a == b
    (Lam v1 t1 b1) == (Lam v2 t2 b2) = t1 == t2 && substitute b1 v1 (Var v2) == b2
    (Fa v1 t1 b1) == (Fa v2 t2 b2) = t1 == t2 && substitute b1 v1 (Var v2) == b2
    _ == _ = False

fresh :: Set Var -> Var
fresh conflicts = head . dropWhile (`member` conflicts) $ nameGen 
    where 
        nameGen = V . pack <$> [a : freshName b | b <- [0..] :: [Int], a <- ['a'..'z']]
        freshName b = if b == 0 then "" else show b

free :: Term -> Set Var
free Var{..} = singleton var
free App{..} = free alg `union` free dat
free Lam{..} = delete var $ free body `union` free tpe
free Fa {..} = delete var $ free body `union` free tpe
free Uni{..} = empty

bound :: Term -> Set Var
bound term = 
    case term of
        Var{..} -> empty
        App{..} -> bound alg `union` bound dat
        Lam{..} -> insert var $ bound body `union` bound tpe
        Fa {..} -> insert var $ bound body `union` bound tpe
        Uni{..} -> empty

alpha :: Term -> Set Var -> Term 
alpha term conflicts = 
    case term of 
        Var{}                   -> term
        Uni{}                   -> term
        App{..}                 -> App (alpha alg conflicts) (alpha dat conflicts)
        Lam{..} |  isConflict   -> Lam var' tpe body'
                |  otherwise    -> Lam var tpe (alpha body conflicts)
        Fa {..} |  isConflict   -> Fa var' tpe body'
                |  otherwise    -> Fa var tpe (alpha body conflicts)
    where 
        isConflict = var term `member` conflicts
        var'       = fresh $ conflicts `union` free (body term) `union` free (tpe term)
        body'      = substitute (body term) (var term) (Var var')

substitute :: Term -> Var -> Term -> Term
substitute term name newTerm = 
    case term of
        Uni{}                               ->  term
        Var{..} | var == name               ->  newTerm 
            | otherwise                 ->  Var var 
        App{..}                             ->  App (subsNewTerm alg) (subsNewTerm dat)
        Lam{var = var'}     | var' == name  ->  term
                            | otherwise     ->  let Lam{..} = term' 
                                                in  Lam var (subsNewTerm tpe) (subsNewTerm body)
        Fa {var = var'}     | var' == name  ->  term
                            | otherwise     ->  let Fa {..} = term' 
                                                in  Fa  var (subsNewTerm tpe) (subsNewTerm body)
    where 
    term'               = alpha term $ free newTerm 
    subsNewTerm oldTerm = substitute oldTerm name newTerm

beta :: Term -> Term
beta term = 
    case term of
        Uni{}               -> term 
        Var{}               -> term 
        (App Lam{..} dat )  -> substitute body var dat
        App{..}             ->  let alg' = beta alg 
                                    dat' = beta dat 
                                in  if alg' == alg 
                                    then App alg dat'
                                    else App alg' dat
        Lam{..}             -> mkBeta
        Fa {..}             -> mkBeta
    where
        mkBeta =    let tpe'    = beta $ tpe term
                        body'   = beta $ body term
                    in  if  tpe term == tpe' 
                        then term {tpe  = tpe'}
                        else term {body = body'}
eta :: Term -> Term
eta term = 
    case term of
        Uni{}   -> term
        Var{}   -> term
        App{..} -> App (eta alg) (eta dat)
        Lam{..} ->  
            case body of 
                App alg Var{var = v} | v == var && v `notMember` free alg   -> eta alg
                _                                                           -> Lam var (eta tpe) (eta body)
        Fa {..} ->  if var == noname 
                    then eta body 
                    else Fa var (eta tpe) (eta body)     

reduce :: Term -> Term 
reduce term =   let term' = beta term 
                in  if   term' == term 
                    then eta term 
                    else reduce term'