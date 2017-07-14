module LawerHL.ParserHL where

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

parserVarTermPairMeta :: Parser (Var, Term)
parserVarTermPairMeta = do var <- parserMetaVar
                           term <- (string ":") *> parserTerm
                           return (var, term)

parserVarTermPair :: Parser (Var, Term)
parserVarTermPair = do pair <- (skipMany spaceChar) *> between (char '(') (char ')') parserVarTermPairMeta <* (skipMany spaceChar)
                       return pair

parseIndParams :: Parser [(Var, Term)]
parseIndParams = do items <- some parserVarTermPair
                    return items

parseInductive :: Parser Inductive
parseInductive = do (V name) <- skipMany spaceChar *> string "inductive" *> skipSome spaceChar *> parserMetaVar <* skipMany spaceChar
                    params <- parseIndParams
                    conss <- string "=" *> parseIndParams
                    return $ Inductive name (Context params) (Context conss)

parseMyInductive :: String -> IO ()
parseMyInductive = parseTest parseInductive . pack

parseMyPair :: String -> IO ()
parseMyPair = parseTest parserVarTermPair . pack