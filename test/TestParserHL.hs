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
    testInd

parseConstruction :: String -> Maybe Construction
parseConstruction = parseMaybe parserConst . pack

parserAlgebraicM :: String -> Maybe Algebraic
parserAlgebraicM = parseMaybe parserAlgebraic . pack

parserInductiveM :: String -> Maybe Inductive
parserInductiveM = parseMaybe parserInductive . pack

testAlg :: Spec
testAlg = it "Parse Alg" $ do
    parserAlgebraicM "data A" `shouldBe` Just (Algebraic {algName = pack "A", algParams = [], algConses = Context []})
    parserAlgebraicM "data A = a | b" `shouldBe` Just  (Algebraic {algName = pack "A", algParams = [], algConses = Context {getCtx = [(V $ pack "a",[]),(V $ pack "b",[])]}})
    parserAlgebraicM "data A a b" `shouldBe` Just  (Algebraic {algName = pack "A", algParams = [pack "a", pack "b"], algConses = Context []})
    parserAlgebraicM "data A b d = b | a b" `shouldBe` Just  (Algebraic {algName = pack "A", algParams = [pack "b", pack "d"], algConses = Context {getCtx = [(V $ pack "b",[]),(V $ pack "a",[TVar $ pack "b"])]}})
    parserAlgebraicM "data A b d c x = b c x | a (c x) b" `shouldBe` Just  (Algebraic {algName = pack "A", algParams = [pack "b", pack "d", pack "c", pack "x"], algConses = Context {getCtx = [(V $ pack "b",[TVar $ pack "c",TVar $ pack "x"]),(V $ pack "a",[TApp (TVar $ pack "c") (TVar $ pack "x"),TVar $ pack "b"])]}})
    parserAlgebraicM "data A b d c x = b c x | a (c x t) b" `shouldBe` Just  (Algebraic {algName = pack "A", algParams = [ pack "b", pack "d", pack "c", pack "x"], algConses = Context {getCtx = [(V $ pack "b",[TVar $ pack "c",TVar $ pack "x"]),(V $ pack "a",[TApp (TApp (TVar $ pack "c") (TVar $ pack "x")) (TVar $ pack "t"),TVar $ pack "b"])]}})
    parserAlgebraicM "data Data Data = a | b" `shouldBe`Just  (Algebraic {algName = pack "Data", algParams = [pack "Data"], algConses = Context {getCtx = [(V $ pack "a",[]),(V $ pack "b",[])]}})
    parserAlgebraicM "data A = a |" `shouldBe` Nothing
    parserAlgebraicM "data A =" `shouldBe` Nothing
    parserAlgebraicM "data = a b z" `shouldBe` Nothing
    parserAlgebraicM "data 1" `shouldBe` Nothing
testAlg = do
    it "Parse Alg1" $ parserAlgebraicM "data A" `shouldBe` Just (Algebraic {algName = pack "A", algParams = [], algConstructors = Context []})
    it "Parse Alg2" $ parserAlgebraicM "data A = a | b" `shouldBe` Just ((Algebraic {algName = pack "A", algParams = [], algConstructors = Context {getCtx = [(V $ pack "a",[]),(V $ pack "b",[])]}}))
    it "Parse Alg3" $ parserAlgebraicM "data A a b" `shouldBe` Just (Algebraic {algName = pack "A", algParams = [pack "a", pack "b"], algConstructors = Context []})
    it "Parse Alg4" $ parserAlgebraicM "data A b d = b | a b" `shouldBe` Just (Algebraic {algName = pack "A", algParams = [pack "b", pack "d"], algConstructors = Context {getCtx = [(V $ pack "b",[]),(V $ pack "a",[TVar $ pack "b"])]}})
    it "Parse Alg5" $ parserAlgebraicM "data A b d c x = b c x | a (c x) b" `shouldBe` Just (Algebraic {algName = pack "A", algParams = [pack "b", pack "d", pack "c", pack "x"], algConstructors = Context {getCtx = [(V $ pack "b",[TVar $ pack "c",TVar $ pack "x"]),(V $ pack "a",[TApp (TVar $ pack "c") (TVar $ pack "x"),TVar $ pack "b"])]}})
    it "Parse Alg6" $ parserAlgebraicM "data A b d c x = b c x | a (c x t) b" `shouldBe` Just (Algebraic {algName = pack "A", algParams = [ pack "b", pack "d", pack "c", pack "x"], algConstructors = Context {getCtx = [(V $ pack "b",[TVar $ pack "c",TVar $ pack "x"]),(V $ pack "a",[TApp (TApp (TVar $ pack "c") (TVar $ pack "x")) (TVar $ pack "t"),TVar $ pack "b"])]}})
    it "Parse Alg7" $ parserAlgebraicM "data Data Data = a | b" `shouldBe`Just (Algebraic {algName = pack "Data", algParams = [pack "Data"], algConstructors = Context {getCtx = [(V $ pack "a",[]),(V $ pack "b",[])]}})
    it "Parse Alg8" $ parserAlgebraicM "data A = a |" `shouldBe` Nothing
    it "Parse Alg9" $ parserAlgebraicM "data A =" `shouldBe` Nothing
    it "Parse Alg10" $ parserAlgebraicM "data = a b z" `shouldBe` Nothing
    it "Parse Alg11" $ parserAlgebraicM "data 1" `shouldBe` Nothing


testInd :: Spec
testInd = it "Parse Ind" $ do
    parserInductiveM "inductive A (a:x)(b:*) = (A1:x)(B1:[])" `shouldBe` Just (Inductive{indName = pack "A", indParams = Context [(V . pack $ "a", Var $ V $ pack "x"), (V $ pack $ "b", Uni Star)], indConses = Context [(V $ pack $ "A1", Var $ V $ pack "x"),(V $ pack "B1",(Uni $ Star))]})
    parserInductiveM "inductive A (a:x)(b:*) = (A1:x)(B1:[])" `shouldBe` Just Inductive{indName = pack "A", indParams = Context [(V . pack $ "a", Var $ V $ pack "x"), (V $ pack "b", Uni Star)], indConstructors = Context [(V $ pack "A1", Var $ V $ pack "x"),(V $ pack "B1", Uni Star)]}

-- testRec :: Spec
-- testRec = it "Parse Rec" $ do
    -- parseConstruction "record A (a:x)(b:*) = (A1:x)(B1:[])" `shouldBe` Just (Rec(Record{recName = pack "A", recParams = Context [(V . pack $ "a", Var $ V $ pack "x"), (V $ pack $ "b", Uni Star)], recConses = Context [(V $ pack $ "A1", Var $ V $ pack "x"),(V $ pack "B1",(Uni $ Star))]}))