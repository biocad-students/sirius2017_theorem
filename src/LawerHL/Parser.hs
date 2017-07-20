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
import Debug.Trace

emptyline :: Parser Char
emptyline = newline *> skipMany (spaceChar <|> tab) *> newline

emptyline2 :: Parser Char
emptyline2 = skipMany (spaceChar <|> tab) *> newline

parserName :: Parser Name
parserName = do (V name) <- parserMetaVar <* skipMany indent
                return name

parserTAppInBr :: Parser TypeApp
parserTAppInBr = do args <- skipMany indent *> between (char '(') (char ')') parserTAppNorm <* skipMany indent
                    return $ makeTApp args
                    where 
                      makeTApp :: [TypeApp] -> TypeApp
                      makeTApp (x:xs) = Prelude.foldl TApp x xs


parserTAppNorm :: Parser [TypeApp]
parserTAppNorm =  skipMany indent *> many parserTApp <* skipMany indent


parserTAppVar :: Parser TypeApp
parserTAppVar = do var <- skipMany indent *> parserName <* skipMany indent
                   return $ TVar var

parserTApp :: Parser TypeApp
parserTApp = try parserTAppInBr <|> try parserTAppVar

parserOneArg :: Parser (Var, [TypeApp])
parserOneArg = do var <- skipMany indent *> parserName <* skipMany indent
                  argss <- many parserTApp <* skipMany indent
                  return  (V var, argss)

parserManyArgs :: Parser [(Var, [TypeApp])]
parserManyArgs = parserOneArg `sepBy1` char '|'

parserAlg :: Parser Algebraic
parserAlg = do name <- skipMany indent *> string "data" *> skipSome indent *> parserName <* skipMany indent
               tpeargs <- many parserName
               char '='
               args <- parserManyArgs
               skipMany indent
               return $ Algebraic name tpeargs (Context args)

parserAlgSimple :: Parser Algebraic
parserAlgSimple = do name <- skipMany indent *> string "data" *> skipSome indent *> parserName <* skipMany indent
                     tpeargs <- many parserName 
                     skipMany indent
                     return $ Algebraic name tpeargs (Context [])

parserAlgebraic :: Parser Algebraic
parserAlgebraic = try parserAlg <|> try parserAlgSimple

parserVarTermPairMeta :: Parser (Var, Term)
parserVarTermPairMeta = do var <- skipMany indent *> parserMetaVar <* skipMany indent
                           term <- (string ":") *> parserTerm <* skipMany indent
                           return (var, term)

parserVarTermPair :: Parser (Var, Term)
parserVarTermPair = do pair <- skipMany indent *> between (char '(') (char ')') parserVarTermPairMeta <* skipMany indent
                       return pair

parserParams :: Parser [(Var, Term)]
parserParams = do items <- many parserVarTermPair
                  return items

parserInductive :: Parser Inductive
parserInductive = do name <- skipMany indent *> string "inductive" *> skipSome indent *> parserName <* skipMany indent
                     params <- parserParams
                     char '='
                     conss <- parserParams
                     skipMany indent
                     return $ Inductive name (Context params) (Context conss)

parserRecord :: Parser Record
parserRecord = do name <- skipMany indent *> string "record" *> skipSome indent *> parserName <* skipMany indent
                  params <- parserParams
                  char '='
                  conss <- parserParams
                  skipMany indent
                  return $ Record name (Context params) (Context conss)

parserConst :: Parser Construction
parserConst = try (Alg <$> parserAlgebraic) <|> try (Ind <$> parserInductive) <|> try (Rec <$> parserRecord)

parseConstruction :: String -> IO ()
parseConstruction = parseTest parserConst . pack

parseMyData :: String -> IO ()
parseMyData = parseTest parserAlgebraic . pack

parseMyInductive :: String -> IO ()
parseMyInductive = parseTest parserInductive . pack

parseMyRecord :: String -> IO ()
parseMyRecord = parseTest parserRecord . pack