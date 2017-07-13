module Main where

import Lawer
import Test.Hspec

main :: IO ()
main = hspec testsParser 

testsParser :: SpecWith ()
testsParser = describe "Parser test" $ do
    testVar

testUni :: Spec
testUni = it "Parse Uni" $ do
	parseTermM "*" `shouldBe` Just (Uni Star)
	parseTermM "[]" `shouldBe` Just (Uni $ Box 1)
