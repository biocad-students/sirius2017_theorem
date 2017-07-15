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
    parseTermM "data A b d = b | a b" `shouldBe` Just (Alg (Algebraic {algName = "A", algParams = ["b","d"], algConses = Context {getCtx = [(V "b",[]),(V "a",[TVar "b"])]}}))
    parseTermM "data A b d c x = b c x | a (c x) b" `shouldBe` Just (Alg (Algebraic {algName = "A", algParams = ["b","d","c","x"], algConses = Context {getCtx = [(V "b",[TVar "c",TVar "x"]),(V "a",[TApp (TVar "c") (TVar "x"),TVar "b"])]}}))
    parseTermM "data A b d c x = b c x | a (c x t) b" `shouldBe` Just (Alg (Algebraic {algName = "A", algParams = ["b","d","c","x"], algConses = Context {getCtx = [(V "b",[TVar "c",TVar "x"]),(V "a",[TApp (TApp (TVar "c") (TVar "x")) (TVar "t"),TVar "b"])]}}))

testInd :: Spec
testInd = it "Parse Ind" $ do
    parseTermM "inductive A (a:x)(b:*) = (A1:x->y->x)(B1:*->*)" `shouldBe` Just (Ind(Inductive{indName = "A", indParams = ["a""b"], indConses = Context {getCtx = [(V "A1",[TVar "x->y->x"]),(V "B1",[TVar "*->*"])]}}))

testRec :: Spec
testRec = it "Parse Rec" $ do
    parseTermM "record A (a:x)(b:*) = (A1:x->y->x)(B1:*->*)" `shouldBe` Just (Rec(Record{recName = "A", recParams = ["a""b"], recConses = Context {getCtx = [(V "A1",[TVar "x->y->x"]),(V "B1",[TVar "*->*"])]}}))