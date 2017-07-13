module Main where

import Lawer
import Test.Hspec
import Data.Text

main :: IO ()
main = hspec testsParser 

testsParser :: SpecWith ()
testsParser = describe "Parser test" $ do
    testUni

testUni :: Spec
testUni = it "Parse Uni" $ do
    parseTermM "*" `shouldBe` Just (Uni Star)
    parseTermM "[]" `shouldBe` Just (Uni $ Box 1)
    parseTermM "[23]" `shouldBe` Just (Uni $ Box 23)
    parseTermM " 5 " `shouldBe` Nothing

testVar :: Spec
testVar = it "Parse Var" $ do
    parseTermM "x" `shouldBe` Just (Var $ V $ pack "x")
    parseTermM "xz12" `shouldBe` Just (Var $ V $ pack "xz12")
    parseTermM "12" `shouldBe` Nothing

testApp :: Spec
testApp = it "Parse App" $ do
    parseTermM "(a) b" `shouldBe` Just (App (Var $ V $ pack "a") (Var $ V $ pack "b"))
    parseTermM "(sd as)" `shouldBe` Nothing

testLam :: Spec
testLam = it "Parse Lam" $ do
    parseTermM "[x:a]b" `shouldBe` Just (Lam (Var $ V $ pack "x") (Var $ V $ pack "a") (Var $ V $ pack "b"))
    parseTermM "[szx]x" `shouldBe` Nothing

testFa :: Spec
testFa = "Parse Fa" $ do
    parseTermM "(x:a)b" `shouldBe` Just (Fa (Var $ V $ pack "x") (Var $ V $ pack "a") (Var $ V $ pack "b"))
    parseTermM "((x)y:a)z" `shouldBe` Nothing

testHard :: Spec
testHard = "Parse HARD" $ do
    parseTermM "(x:a)[y:b](c2)d" `shouldBe` Just (Fa (Var $ V $ pack "x") (Var $ V $ pack "a") 
                                                 (Lam (Var $ V $ pack "y") (Var $ V $ pack "b") 
                                                 (App (Var $ V $ pack "c2") (Var $ V $ pack "d"))))
