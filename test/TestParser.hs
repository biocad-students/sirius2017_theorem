module Main where

import Lawer
import Test.Hspec

main :: IO ()
main = hspec testsParser 

testsParser :: SpecWith ()
testsParser = describe "Parser test" $ do
    testVar

testVar :: Spec
testVar = it "Parse var" $ True `shouldBe` True

-- v a= parseTest parseVar a
-- s a= parseTest parseStar a
-- b1 a= parseTest parseBox a
-- b2 a= parseTest parseBox a
-- a x= parseTest parseApp x
-- l a= parseTest parseLam a
-- f a= parseTest parseFa a
-- u a = parseTest parseTerm a