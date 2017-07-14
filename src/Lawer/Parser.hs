{-# LANGUAGE OverloadedStrings #-}
module Lawer.Parser where

import Control.Applicative   (many, some, (<|>))
import Text.Megaparsec
import Text.Megaparsec.Text  (Parser)
import Text.Megaparsec.Lexer
import Lawer.Type
import Lawer.Pretty
import Data.Text


parserBoxInt :: Parser Integer
parserBoxInt = read <$> (some digitChar <|> pure "1")

parserVar :: Parser Term
parserVar = do st <- skipMany spaceChar *> some letterChar
               dg <- many digitChar <* skipMany spaceChar
               return $ Var $ V $ pack (st ++ dg)

parserApp :: Parser Term
parserApp = do term1 <- skipMany spaceChar *> between (char '(') (char ')') (skipMany spaceChar *> parserTerm <* skipMany spaceChar)
               term2 <- skipMany spaceChar *> parserTerm
               return $ App term1 term2

parserTermInBr :: Parser Term
parserTermInBr = between (char '(') (char ')') parserTerm

parserTerm :: Parser Term
parserTerm = try parserLam <|> try parserApp <|>  try parserTermInBr <|> try parserFa <|> try parserVar <|> try parserUni <* parserSpaces

parserStar :: Parser Term
parserStar = do star <- skipMany spaceChar *> char '*' <* skipMany spaceChar
                return $ Uni Star

parserUni :: Parser Term
parserUni = try parserStar <|> try parserBox

parserBox :: Parser Term
parserBox = do num <- skipMany spaceChar *> string "[" *> parserBoxInt <* string "]" <* skipMany spaceChar
               return $ Uni $ Box num


parserLamMeta = do Var var <- parserVar
                   tpe <- string ":" *> parserTerm <* skipMany spaceChar
                   return $ Lam var tpe


parserFaMeta = do (Var var) <- parserVar
                  tpe <- string ":" *> parserTerm <* skipMany spaceChar
                  return $ Fa var tpe


parserLam :: Parser Term
parserLam = do meta <- skipMany spaceChar *> between (char '[') (char ']') parserLamMeta
               term <- parserTerm
               return $ meta term

parserSpaces :: Parser ()
parserSpaces = skipMany spaceChar *> eof

parserFa :: Parser Term
parserFa = do meta <- skipMany spaceChar *> between (char '(') (char ')') parserFaMeta
              term <- parserTerm
              return $ meta term


parseTermM :: String -> Maybe Term
parseTermM = parseMaybe parserTerm . pack

parseTerm :: String -> Either String Term
parseTerm s = case parse parserTerm "CoC parser" (pack s) of
  Right term ->Right term
  Left err -> Left $ parseErrorPretty err

parseNprintTerm :: String -> IO ()
parseNprintTerm = parseTest parserTerm . pack
