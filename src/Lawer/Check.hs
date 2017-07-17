{-# LANGUAGE RecordWildCards #-}

module Lawer.Check (typeOf) where 

import Lawer.Type
import Lawer.Reduce
import Lawer.Context 
import Lawer.Error                         (CalculusError(..))
import Control.Monad.Trans.Except          (Except, throwE)
import Prelude                      hiding (lookup)

typeOf :: Term -> Except CalculusError Term 
typeOf t = reduce <$> typeWith empty t

typeWith :: Context Term -> Term -> Except CalculusError Term
typeWith ctx term = 
    case term of
        Var{..} -> case lookup var ctx of
                       Just x  -> return x
                       Nothing -> throwE $ UnknownVariable var
        Uni{..} -> return . Uni $ axiom uni
        App{..} -> 
            do  algTpe <- reduce <$> typeWith ctx (reduce alg)
                case algTpe of
                    Fa{..} -> 
                        do
                            datTpe <- reduce <$> typeWith ctx dat
                            if isIn (reduce tpe) (reduce datTpe) ctx
                            then return $ substitute body var dat
                            else throwE $ CannotEqualizeTypes (reduce tpe) (reduce datTpe)
                    _      -> throwE $ InvalidType algTpe "must be arrow"
        Lam{..} ->  do  let ctx' = insert var tpe ctx 
                        bodyTpe <- typeWith ctx' body
                        let lamTpe = Fa var tpe bodyTpe
                        _ <- typeWith ctx lamTpe
                        return lamTpe
        Fa {..} ->  do  let ctx' = insert var tpe ctx
                        typeTpe <- (reduce <$> typeWith ctx tpe) >>= toUni
                        bodyTpe <- (reduce <$> typeWith ctx' body) >>= toUni
                        return . Uni $ typeRule typeTpe bodyTpe

isIn :: Term -> Term -> Context Term -> Bool
isIn (Var x) (Var y) _                  = x == y
isIn (Var x) u@Uni{} ctx                =   case lookup x ctx of
                                                Just t -> isIn t u ctx 
                                                _      -> False
isIn (App x y) (App a b) ctx            = isIn x a ctx && isIn y b ctx
isIn (Uni a) (Uni b) _                  = a <= b
isIn (Lam v1 t1 b1) (Lam v2 t2 b2) ctx  = isIn t1 t2 ctx && isIn (substitute b1 v1 (Var v2)) b2 ctx
isIn (Fa v1 t1 b1) (Fa v2 t2 b2) ctx    = isIn t1 t2 ctx && isIn (substitute b1 v1 (Var v2)) b2 ctx
isIn _ _ _                              = False

                           
toUni :: Term -> Except CalculusError Uni
toUni (Uni u) = pure u
toUni a       = throwE $ InvalidType a "must be uni"     

typeRule :: Uni -> Uni -> Uni 
typeRule Star u = u 
typeRule _ Star = Star 
typeRule (Box i) (Box j) = Box $ max i j