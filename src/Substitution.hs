module Substitution where 

import Type
import Data.List
import Reduce

freshType :: [Var] -> Var
freshType arr = head $ filter (`notElem` arr) a
            where a = [V $ x : show i | i <- [1..] :: [Int], x <- ['A'..'Z']]

composeSubs :: Substitution -> Substitution -> Substitution
composeSubs a b = map (\(TS x t) -> TS x $ subsT a t) b ++ a

subsT :: Substitution -> Type -> Type
subsT [] a                                  = a
subsT ((TS b sub):xs) (Type a) | a == b     = sub 
                               | otherwise  = subsT xs (Type a)
subsT arr (Arrow a b) = Arrow (subsT arr a) (subsT arr b)

algU' :: Type -> Type -> Substitution
algU' a@(Type x) b@(Type _) | a == b                = []
                            | otherwise             = [TS x b]
algU' (Type x) b            | x `elem` freeTypes b  = error "error"
                            | otherwise             = [TS x b] 
algU' a b@(Type _)                                  = algU' b a
algU' (Arrow a b) (Arrow c d)                       = algU' (subsT f a) (subsT f c) `composeSubs` f
                                                            where f = algU' b d

algU :: [(Type, Type)] -> Substitution
algU [] = []
algU ((Arrow a b, Arrow c d):xs) = algU ((a, c) : (b, d) : xs)
algU ((a, b):xs) =  ab `composeSubs` algU (map f xs) 
                    where   ab = algU' a b
                            f = \(x, y) -> ((subsT ab x), (subsT ab y))

algE :: Context -> Term -> Type -> [(Type, Type)]
algE g x a = algE' g x a $ freeTypes a

algE' :: Context -> Term -> Type -> [Var] -> [(Type, Type)]
algE' g (Var x) a _ =   case lookupContext x g of
                            Just f -> (a, f):[]
                            Nothing -> []
algE' g (App a b) t arr =   let fr = freshType $ arr
                            in algE' g a (Arrow (Type fr) t) (fr:arr) ++ algE' g b (Type fr) (fr:arr)
algE' g@(Context ctx) (Lam a b) t arr = let fo = freshType arr
                                            ft = freshType $ fo:arr
                                        in (t, Arrow (Type fo) (Type ft)) : algE' (Context $ (a, Type fo) : ctx) b (Type ft) (fo:ft:arr)

algPP :: Term -> (Context, Type)
algPP term = let func = reverse . algU . reverse . algE (Context gamma0) term $ sigma0
                 fr = free term
                 gamma0 = zip fr $ (map Type . take (length fr) $ arr)
                 arr = [V $ x : show i | i <- [1..] :: [Int], x <- ['A'..'Z']] 
                 sigma0 = Type $ arr !! (length fr)
                 gamma = Context $ map (\(a, b) -> (a, subsT func b)) gamma0
                 sigma = subsT func sigma0
             in (gamma, sigma) 

freeTypes :: Type -> [Var]
freeTypes (Type a) = [a]
freeTypes (Arrow a b) = nub $ freeTypes a ++ freeTypes b

lookupContext :: Var -> Context -> Maybe Type 
lookupContext t (Context a) = lookup t a