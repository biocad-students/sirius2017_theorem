module LawerHL.Parser (parseMyPair, parserVarTermPair) where

import Control.Applicative   (many, some, (<|>))
import Text.Megaparsec
import Text.Megaparsec.Text  (Parser)
import Text.Megaparsec.Lexer
import Lawer.Type
import Lawer.Pretty
import Lawer.Parser
import Data.Text

parserVarTermPairMeta :: Parser (Term, Term)
parserVarTermPairMeta = do var <- parserVar
                           term <- string ":" *> parserTerm
                           return (var, term)

parserVarTermPair :: Parser (Term, Term)
parserVarTermPair = skipMany spaceChar *> between (char '(') (char ')') parserVarTermPairMeta <* skipMany spaceChar

                       
parseMyPair :: String -> IO ()
parseMyPair = parseTest parserVarTermPair . pack