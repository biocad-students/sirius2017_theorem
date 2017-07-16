module LawerHL.Parser where

import Control.Applicative   (many, some, (<|>))
import Text.Megaparsec
import Text.Megaparsec.Text  (Parser)
import Text.Megaparsec.Lexer
import Lawer.Type
import Lawer.Pretty
import Lawer.Parser
import Lawer.Context
import LawerHL.Pretty
import LawerHL.Type
import Data.Text

parserName :: Parser Name
parserName = do (V name) <- parserMetaVar
                return name

parserTAppInBr :: Parser TypeApp
parserTAppInBr = do args <- skipMany spaceChar *> between (char '(') (char ')') (skipMany spaceChar *> parserTAppNorm <* skipMany spaceChar) <* skipMany spaceChar
                    return $ makeTApp args
                    where 
                      makeTApp :: [TypeApp] -> TypeApp
                      makeTApp (x:xs) = Prelude.foldl TApp x xs


parserTAppNorm :: Parser [TypeApp]
parserTAppNorm =  many parserTApp


parserTAppVar :: Parser TypeApp
parserTAppVar = do var <- parserName
                   return $ TVar var

parserTApp :: Parser TypeApp
parserTApp = try parserTAppInBr <|> try parserTAppVar

parserOneArg :: Parser (Var, [TypeApp])
parserOneArg = do var <- parserName
                  argss <- many parserTApp
                  return  (V var, argss)

parserManyArgs :: Parser [(Var, [TypeApp])]
parserManyArgs =  parserOneArg `sepBy` char '|' <* parserSpaces

parserAlg :: Parser Algebraic
parserAlg = do name <- skipMany spaceChar *> string "data" *> skipSome spaceChar *> parserName <* skipMany spaceChar
               tpeargs <- many parserName
               args <- char '=' *> skipMany spaceChar *> parserManyArgs <* skipMany spaceChar
               return $ Algebraic name tpeargs (Context args)

parserAlgSimple :: Parser Algebraic
parserAlgSimple = do name <- skipMany spaceChar *> string "data" *> skipSome spaceChar *> parserName <* skipMany spaceChar
                     tpeargs <- many parserName
                     return $ Algebraic name tpeargs (Context [])

parserAlgebraic :: Parser Algebraic
parserAlgebraic = (try parserAlg <|> try parserAlgSimple) <* parserSpaces

parserVarTermPairMeta :: Parser (Var, Term)
parserVarTermPairMeta = do var <- parserMetaVar
                           term <- (string ":") *> parserTerm
                           return (var, term)

parserVarTermPair :: Parser (Var, Term)
parserVarTermPair = do pair <- (skipMany spaceChar) *> between (char '(') (char ')') parserVarTermPairMeta <* (skipMany spaceChar)
                       return pair

parserParams :: Parser [(Var, Term)]
parserParams = do items <- many parserVarTermPair
                  return items

parserInductive :: Parser Inductive
parserInductive = do (V name) <- skipMany spaceChar *> string "inductive" *> skipSome spaceChar *> parserMetaVar <* skipMany spaceChar
                     params <- parserParams
                     conss <- string "=" *> parserParams
                     return $ Inductive name (Context params) (Context conss)

parserRecord :: Parser Record
parserRecord = do (V name) <- skipMany spaceChar *> string "record" *> skipSome spaceChar *> parserMetaVar <* skipMany spaceChar
                  params <- parserParams
                  conss <- string "=" *> parserParams
                  return $ Record name (Context params) (Context conss)

parserConst :: Parser Construction
parserConst = try (Alg <$> parserAlgebraic) <|> try (Ind <$> parserInductive) <|> try (Rec <$> parserRecord) <* parserSpaces

parseConstruction :: String -> IO ()
parseConstruction = parseTest parserConst . pack

parseMyData :: String -> IO ()
parseMyData = parseTest parserAlgebraic . pack

parseMyInductive :: String -> IO ()
parseMyInductive = parseTest parserInductive . pack

parseMyRecord :: String -> IO ()
parseMyRecord = parseTest parserRecord . pack