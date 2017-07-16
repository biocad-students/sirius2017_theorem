module Main where

import Lawer
import LawerHL
import Test.Hspec
import Data.Text
import Text.Megaparsec

main :: IO ()
main = hspec testsParserHL

testsParserHL :: SpecWith ()
testsParserHL = describe "Parser HL test" $ do  
    testAlg

parseConstruction :: String -> Maybe Construction
parseConstruction = parseMaybe parserConst . pack

testAlg :: Spec
testAlg = it "Parse Alg" $ do
    parseConstruction "data A" `shouldBe` Just (Alg (Algebraic {algName = pack "A", algParams = [], algConses = Context []}))
    parseConstruction "data A = a | b" `shouldBe` Just (Alg (Algebraic {algName = pack "A", algParams = [], algConses = Context {getCtx = [(V $ pack "a",[]),(V $ pack "b",[])]}}))
    parseConstruction "data A a b" `shouldBe` Just (Alg (Algebraic {algName = pack "A", algParams = [pack "a", pack "b"], algConses = Context []}))
    parseConstruction "data A b d = b | a b" `shouldBe` Just (Alg (Algebraic {algName = pack "A", algParams = [pack "b", pack "d"], algConses = Context {getCtx = [(V $ pack "b",[]),(V $ pack "a",[TVar $ pack "b"])]}}))
    parseConstruction "data A b d c x = b c x | a (c x) b" `shouldBe` Just (Alg (Algebraic {algName = pack "A", algParams = [pack "b", pack "d", pack "c", pack "x"], algConses = Context {getCtx = [(V $ pack "b",[TVar $ pack "c",TVar $ pack "x"]),(V $ pack "a",[TApp (TVar $ pack "c") (TVar $ pack "x"),TVar $ pack "b"])]}}))
    parseConstruction "data A b d c x = b c x | a (c x t) b" `shouldBe` Just (Alg (Algebraic {algName = pack "A", algParams = [ pack "b", pack "d", pack "c", pack "x"], algConses = Context {getCtx = [(V $ pack "b",[TVar $ pack "c",TVar $ pack "x"]),(V $ pack "a",[TApp (TApp (TVar $ pack "c") (TVar $ pack "x")) (TVar $ pack "t"),TVar $ pack "b"])]}}))
    parseConstruction "data Data Data = a | b" `shouldBe`Just (Alg (Algebraic {algName = pack "Data", algParams = [pack "Data"], algConses = Context {getCtx = [(V $ pack "a",[]),(V $ pack "b",[])]}}))
    parseConstruction "data A = a |" `shouldBe` Nothing
    parseConstruction "data A =" `shouldBe` Nothing
    parseConstruction "data = a b z" `shouldBe` Nothing
    parseConstruction "data 1" `shouldBe` Nothing


--testInd :: Spec
--testInd = it "Parse Ind" $ do
--    parseConstruction "inductive A (a:x)(b:*) = (A1:x->y->x)(B1:*->*)" `shouldBe` Just (Ind(Inductive{indName = "A", indParams = ["a""b"], indConses = Context {getCtx = [(V "A1",[TVar "x->y->x"]),(V "B1",[TVar "*->*"])]}}))

--testRec :: Spec
--testRec = it "Parse Rec" $ do
--    parseConstruction "record A (a:x)(b:*) = (A1:x->y->x)(B1:*->*)" `shouldBe` Just (Rec(Record{recName = "A", recParams = ["a""b"], recConses = Context {getCtx = [(V "A1",[TVar "x->y->x"]),(V "B1",[TVar "*->*"])]}}))