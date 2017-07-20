{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}

module LawerHL.Encoding (constructionToTerm) where

import LawerHL.Type
import Lawer.Type 
import Lawer.Context 
import Lawer.Check 
import Lawer.Reduce 
import Lawer.Sugar 
import Data.Text (pack)

constructionToTerm :: Construction -> Context Term 
constructionToTerm (Ind cons) = inductiveToTerm cons
constructionToTerm (Rec cons) = recordToTerm cons
constructionToTerm (Alg cons) = encodeAlgebraic cons

inductiveToTerm :: Inductive -> Context Term 
inductiveToTerm Inductive{..} = insert (V indName) getInductiveType getConstructors
    where 
        getInductiveType :: Term
        getInductiveType = foldParams $
                           Fa uniVar foldParamsTypes $
                           foldConstructorsTypes Fa foldAppType
        
        foldParams :: Term -> Term 
        foldParams term = let nextLam (v, t) = Lam v t
                          in  foldr nextLam term $ getCtx indParams

        foldParamsTypes :: Term 
        foldParamsTypes = foldr1 (-->) $ map snd $ getCtx indParams ++ [(noname, Uni Star)]

        foldConstructorsTypes :: (Var -> Term -> Term -> Term) -> Term -> Term
        foldConstructorsTypes cons term = let nextCons (v, t) = cons v (substitute t (V indName) (Var uniVar))
                                          in  foldr nextCons term $ getCtx indConstructors

        foldAppType :: Term 
        foldAppType = let ctx = (V indName, Var noname) : getCtx indParams
                      in  foldl1 ($$) $ map (Var . fst) ctx

        getConstructors :: Context Term 
        getConstructors = Context . map getConstructor $ getCtx indConstructors

        getConstructor :: (Var, Term) -> (Var, Term)
        getConstructor (name, type') = (,) name $
                                       fromTypeToArgs type' $ 
                                       Lam uniVar foldParamsTypes $
                                       foldConstructorsTypes Lam 
                                       getResultTerm
            where
                fromTypeToArgs :: Term -> Term -> Term
                fromTypeToArgs Fa{..} term = Lam var tpe $ fromTypeToArgs body term
                fromTypeToArgs t      term = term 
                
                getResultTerm :: Term 
                getResultTerm = fromTypeToApp $ Fa name (Var noname) type'

                fromTypeToApp :: Term -> Term
                fromTypeToApp Fa{..} = 
                    case body of
                        Fa{} -> Var var $$ fromTypeToApp body 
                        _    -> Var var 
        

recordToTerm :: Record -> Context Term 
recordToTerm Record{..} = inductiveToTerm (Inductive recName recParams recConstructors)

encodeAlgebraic :: Algebraic -> Context Term
encodeAlgebraic Algebraic{..} = insert (V algName) getAlgebraicType getConstructors 
    where
        algTypeApp = let nextTApp x = TApp x . TVar
                     in  foldl nextTApp (TVar algName) algParams 

        getAlgebraicType :: Term
        getAlgebraicType =  foldParams $ 
                            Fa uniVar (Uni Star) $ 
                            foldConstructorsTypes (getCtx algConstructors) $
                            Var uniVar

        foldParams :: Term -> Term
        foldParams term = let nextLam x = Lam (V x) (Uni Star)
                          in  foldr nextLam term algParams 

        foldConstructorsTypes :: [(Var, [TypeApp])] -> Term -> Term
        foldConstructorsTypes []         t    = t
        foldConstructorsTypes ((_,x):xs) t    = fromTypeApp x True --> 
                                                foldConstructorsTypes xs t

        fromTypeApp :: [TypeApp]  -> Bool -> Term
        fromTypeApp [] _                                                  = Var uniVar
        
        fromTypeApp [TVar x] flag        | TVar x == algTypeApp && flag   = Var uniVar --> Var uniVar
                                         | flag                           = Var (V x) --> Var uniVar
                                         | otherwise                      = Var (V x) 

        fromTypeApp (TVar x : xs) flag   | TVar x == algTypeApp && flag   = Var uniVar --> fromTypeApp xs flag
                                         | otherwise                      = Var (V x)  --> fromTypeApp xs flag 

        fromTypeApp [TApp x y]    flag   | TApp x y == algTypeApp && flag = Var uniVar --> Var uniVar
                                         | otherwise                      = fromTypeApp [x] flag $$ fromTypeApp [y] flag

        fromTypeApp (TApp x y : xs) flag | TApp x y == algTypeApp && flag = Var uniVar --> fromTypeApp xs flag
                                         | otherwise                      = (fromTypeApp [x] flag $$ fromTypeApp [y] flag) --> 
                                                                            fromTypeApp xs flag

        getConstructors :: Context Term
        getConstructors = Context . map getConstructor $ getCtx algConstructors

        getConstructor :: (Var, [TypeApp]) -> (Var, Term)
        getConstructor (name, args) =   let 
                                            countArgs = length args
                                            freshVars = fresh $ map V algParams 

                                            foldArgs :: Term -> Term
                                            foldArgs t = let nextLam (x, y) = Lam x (fromTypeApp [y] True)
                                                         in  foldr nextLam t $ zip freshVars args

                                            getResultTerm :: Term 
                                            getResultTerm = let nextApp a b = a $$ getTerm b
                                                            in  foldl nextApp (Var name) $ zip (take countArgs freshVars) args
                                                            

                                            getTerm :: (Var, TypeApp) -> Term 
                                            getTerm (n, ta) | ta == algTypeApp = foldAppConstructors
                                                            | otherwise        = Var n
                                                where
                                                    foldAppConstructors :: Term 
                                                    foldAppConstructors = let ctx = (n, []) : (uniVar, []) : getCtx algConstructors
                                                                          in  foldl1 ($$) $ map (Var . fst) ctx

                                            foldConstructors :: Term -> Term
                                            foldConstructors t = let nextLam (x, y) = Lam x $ fromTypeApp y True
                                                                 in  foldr nextLam t $ getCtx algConstructors
        
                                        in  
                                            (,) name $ 
                                            foldParams $            -- -->
                                            foldArgs $              -- -->
                                            Lam uniVar (Uni Star) $ -- --> 
                                            foldConstructors        -- -->
                                            getResultTerm


fresh :: [Var] -> [Var]
fresh conflicts = dropWhile (`elem` conflicts) nameGen 
    where 
        nameGen = V . pack <$> [a : freshName b | b <- [0..] :: [Int], a <- ['a'..'z']]
        freshName b = if b == 0 then "" else show b

uniVar :: Var
uniVar = V $ pack "r"