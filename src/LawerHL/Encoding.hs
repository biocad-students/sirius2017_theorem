{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

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
        typeParams = map fst $ getCtx indParams
        
        toBind :: Term -> Context Term
        toBind term =  let freeVars = free term

                           removeFree :: Context Term 
                           removeFree = Context $ filter (\a -> fst a `elem` freeVars) $ getCtx indParams                         

                       in  removeFree 

        subsName :: Term -> Term 
        subsName t = substitute t (V indName) $ Var uniVar

        getInductiveType :: Term 
        getInductiveType =  foldParams indParams $
                            Fa uniVar foldParamsTypes $
                            subsName $
                            foldConstructorsTypes 
                            foldResultType
        
        foldParams ::  Context Term -> Term -> Term
        foldParams ctx term = foldr (uncurry Lam) term $ getCtx ctx

        foldParamsTypes :: Term
        foldParamsTypes = foldr ((-->) . snd) (Uni Star) $ getCtx indParams

        foldConstructorsTypes :: Term -> Term
        foldConstructorsTypes term = foldr ((-->) . snd) term $ getCtx indConstructors
        
        foldResultType :: Term 
        foldResultType = let nextApp a b = ($$) a $ Var $ fst b
                         in  foldl nextApp (Var uniVar) $ getCtx indParams
        
        getConstructors :: Context Term 
        getConstructors = Context . map (uncurry getConstructor) $ getCtx indConstructors

        getConstructor :: Var -> Term -> (Var, Term)
        getConstructor name type' = (,) name $
                                    foldParams (toBind type') $
                                    foldArgs type' typeParams $
                                    Lam uniVar foldParamsTypes $
                                    subsName $
                                    foldConstructors 
                                    foldResultTerm
            where
                foldArgs :: Term -> [Var] -> Term -> Term 
                foldArgs type' arr term = case type' of
                    Fa (V "_") tpe body ->  let fName = head $ fresh arr 
                                                arr'  = fName : arr 
                                            in  Lam fName tpe $
                                                foldArgs body arr' term
                    Fa name    tpe body ->  Lam name tpe $
                                            foldArgs body arr term 
                    _                   ->  term
                
                foldConstructors :: Term -> Term 
                foldConstructors term = foldr (uncurry Lam) term $ getCtx indConstructors

                -- foldResultTerm :: Term 
                -- foldResultTerm = foldl ($$) (Var name) $ getArgs type' typeParams

                getArgs :: Term -> [Var] -> [(Var, Term)] 
                getArgs type' arr = case type' of
                    Fa (V "_") tpe body ->  let fName = head $ fresh arr 
                                                arr'  = fName : arr 
                                            in  (fName, tpe) :
                                                getArgs body arr'
                    Fa name    tpe body ->  (name, tpe) :
                                            getArgs body arr
                    _                   ->  []

                args = getArgs type' typeParams

                foldAppParams :: Term
                foldAppParams = foldl1 ($$) . map (Var . fst) $ getCtx indParams 
                
                foldResultTerm :: Term 
                foldResultTerm = let nextApp a b = a $$ getTerm b
                                 in  foldl nextApp (Var name) args
                                

                getTerm :: (Var, Term) -> Term 
                getTerm (v, t) | t == (Var $ V indName)                   = let leftPart = foldl ($$) (Var uniVar) . map (Var . fst) $ getCtx ctx 
                                                                                ctx      = insert v (Var noname) indParams
                                                                            in  foldl ($$) leftPart $ map (Var . fst) $ getCtx indConstructors 
                               | t == (Var $ V indName) $$ foldAppParams  = let leftPart = foldl ($$) (Var uniVar) . map (Var . fst) $ getCtx ctx 
                                                                                ctx      = insert v (Var noname) indParams
                                                                            in  foldl ($$) leftPart $ map (Var . fst) $ getCtx indConstructors 
                               | otherwise                                = Var v

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
                                            foldArgs t = let nextLam (x, y) = Lam x (fromTypeApp [y] False)
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
uniVar = V "r"