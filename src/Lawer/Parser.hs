{-# LANGUAGE OverloadedStrings #-}
module Lawer.Parser (parseTermM, parseTerm, parserVar, parserTerm, parserMetaVar, parserSpaces, indent) where

import Control.Applicative   (many, some, (<|>))
import Text.Megaparsec
import Text.Megaparsec.Text  (Parser)
import Text.Megaparsec.Lexer
import Lawer.Type
import Lawer.Pretty
import Data.Text

indent :: Parser Char
indent = spaceChar <|> tab <|> newline

parserBoxInt :: Parser Integer
parserBoxInt = read <$> (some digitChar <|> pure "1")

parserVar :: Parser Term
parserVar = do var <- parserMetaVar
               return $ Var var

parserMetaVar :: Parser Var
parserMetaVar = do st <- some letterChar
                   if st == "data" then fail "keywords connot be used for var's name" 
                   else do dg <- many digitChar
                           return $ V $ pack (st ++ dg)

parserApp :: Parser Term
parserApp = do term1 <- skipMany indent *> between (char '(') (char ')') (skipMany indent *> parserTerm <* skipMany indent)
               term2 <- skipMany indent *> parserTerm
               return $ App term1 term2

parserTermInBr :: Parser Term
parserTermInBr = between (char '(') (char ')') parserTerm

parserTerm :: Parser Term
parserTerm = try parserLam <|> try parserApp <|>  try parserTermInBr <|> try parserFa <|> try parserVar <|> try parserUni

parserStar :: Parser Term
parserStar = do star <- skipMany indent *> char '*' <* skipMany indent
                return $ Uni Star

parserUni :: Parser Term
parserUni = try parserStar <|> try parserBox

parserBox :: Parser Term
parserBox = do num <- skipMany indent *> string "[" *> parserBoxInt <* string "]" <* skipMany indent
               return $ Uni $ Box num


parserLamMeta = do Var var <- parserVar
                   tpe <- string ":" *> parserTerm <* skipMany indent
                   return $ Lam var tpe


parserFaMeta = do (Var var) <- parserVar
                  tpe <- string ":" *> parserTerm <* skipMany indent
                  return $ Fa var tpe


parserLam :: Parser Term
parserLam = do meta <- skipMany indent *> between (char '[') (char ']') parserLamMeta
               term <- parserTerm
               return $ meta term

parserSpaces :: Parser ()
parserSpaces = skipMany indent *> eof

parserFa :: Parser Term
parserFa = do meta <- skipMany indent *> between (char '(') (char ')') parserFaMeta
              term <- parserTerm
              return $ meta term


parseTermM :: String -> Maybe Term
parseTermM = parseMaybe parserTermMeta . pack

parserTermMeta :: Parser Term
parserTermMeta = parserTerm <* parserSpaces

parseTerm :: String -> Either String Term
parseTerm s = case parse parserTermMeta "CoC parser" (pack s) of
  Left err -> Left $ parseErrorPretty err
  Right t -> Right t

parseNprintTerm :: String -> IO ()
parseNprintTerm = parseTest parserTermMeta . pack