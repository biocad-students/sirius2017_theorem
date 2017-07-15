module Main where

import Lawer
import LawerHL
import Test.Hspec
import Data.Text

main :: IO ()
main = hspec testsParserHL

testsParserHL :: SpecWith ()
testsParserHL = describe "Parser HL test" $ do
    testAlg

testAlg :: Spec
testAlg = it "Parse Alg" $ do
    parseTermM "data A b d= b | a b" `shouldBe` Just (Alg (Algebraic {algName = "A", algParams = ["b","d"], algConses = Context {getCtx = [(V "b",[]),(V "a",[TVar "b"])]}}))
    parseTermM "data A b d c x = b c x | a (c x) b" `shouldBe` Just (Alg (Algebraic {algName = "A", algParams = ["b","d","c","x"], algConses = Context {getCtx = [(V "b",[TVar "c",TVar "x"]),(V "a",[TApp (TVar "c") (TVar "x"),TVar "b"])]}}))
    parseTermM "data A b d c x = b c x | a (c x t) b" `shouldBe` Just (Alg (Algebraic {algName = "A", algParams = ["b","d","c","x"], algConses = Context {getCtx = [(V "b",[TVar "c",TVar "x"]),(V "a",[TApp (TApp (TVar "c") (TVar "x")) (TVar "t"),TVar "b"])]}}))

testInd :: Spec
testInd = it "Parse Ind" $ do
    parseTermM "" `shouldBe` Nothing

testRec :: Spec
testRec = it "Parse Rec" $ do
    parseTermM "" `shouldBe` Nothing