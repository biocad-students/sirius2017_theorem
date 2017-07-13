{-# LANGUAGE OverloadedStrings #-}
module Lawer.Parser where

import Control.Applicative   (many, some, (<|>))
import Text.Megaparsec
import Text.Megaparsec.Text  (Parser)
import Text.Megaparsec.Lexer
import Lawer
import Data.Text


parseBoxInt :: Parser Integer
parseBoxInt = read <$> (some digitChar <|> pure "1")

parseVar :: Parser Term
parseVar = do st <- (skipMany spaceChar) *> (some letterChar)
              dg <- (many digitChar) <* (skipMany spaceChar)
              return $ Var $ V $ pack (st ++ dg)

parseApp :: Parser Term
parseApp = do term1 <- (skipMany spaceChar) *> between (char '(') (char ')') ((skipMany spaceChar) *> parseTerm <* (skipMany spaceChar))
              term2 <- (skipMany spaceChar) *> parseTerm
              return $ App term1 term2

parseTermInBr :: Parser Term
parseTermInBr = between (char '(') (char ')') parseTerm

parseTerm :: Parser Term
parseTerm = try parseLam <|> try parseApp <|>  try parseTermInBr <|> try parseFa <|> try parseVar <|> try parseUni

parseNotAppTerm :: Parser Term
parseNotAppTerm = try parseLam <|> try parseUni <|> try parseVar

parseStar :: Parser Term
parseStar = do star <- (skipMany spaceChar) *> (char '*') <* (skipMany spaceChar)
               return $ Uni Star

parseUni :: Parser Term
parseUni = try parseStar <|> try parseBox

parseBox :: Parser Term
parseBox = do num <- (skipMany spaceChar) *> (string "[") *> parseBoxInt <* (string "]") <* (skipMany spaceChar)
              return $ Uni $ Box num


parseLamMeta = do (Var var) <- parseVar
                  tpe <- (string ":") *> parseTerm <* (skipMany spaceChar)
                  return $ Lam var tpe


parseFaMeta = do (Var var) <- parseVar
                 tpe <- (string ":") *> parseTerm <* (skipMany spaceChar)
                 return $ Fa var tpe


parseLam :: Parser Term
parseLam = do meta <- (skipMany spaceChar) *> between (char '[') (char ']') parseLamMeta
              term <- parseTerm
              return $ meta term

parseFa :: Parser Term
parseFa = do meta <- (skipMany spaceChar) *> between (char '(') (char ')') parseFaMeta
             term <- parseTerm
             return $ meta term

parseMyTerm :: String -> IO ()
parseMyTerm = parseTest parseTerm . pack
