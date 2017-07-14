module Main where

import Lawer
import Test.Hspec
import Std
import Data.Text (pack)
import Prelude hiding (not, succ)

main :: IO ()
main = hspec testsReduceAndTypes 

testsReduceAndTypes :: SpecWith ()
testsReduceAndTypes = do
    describe "Reduction" $ do
        testA
        testB
        testC
        testD
        testE
    describe "Bool" testBool
    describe "Nat" testNat

testNat :: Spec
testNat = do
    it "0 + 0 = 0" $ reduce (plus `app` zero `app` zero) `shouldBe` reduce zero 
    it "1 + 0 = 1" $ reduce (plus `app` (succ $$ zero) `app` zero) `shouldBe` reduce one
    it "0 + 1 = 1" $ reduce (plus `app` zero `app` one) `shouldBe` reduce one
    it "1 + 1 = 2" $ reduce (plus `app` one `app` one) `shouldBe` reduce two 
    it "2 + 4 = 4 + 2" $ reduce (plus $$ two $$ four) `shouldBe` reduce (plus $$ four $$ two)
    it "2 + 4 =types= 4 + 2" $ typeOf (reduce (App (App plus two) four)) `shouldBe` typeOf (reduce (App (App plus four) two))
    
testBool :: Spec
testBool = do 
    it "not false = true" $ reduce (App not false) `shouldBe` true
    it "not false =types= true" $ typeOf (reduce (App not false)) `shouldBe` typeOf true

testA :: Spec
testA = it "(\\x y -> x) x = \\y -> x" $ 
    reduce (App (Lam x st (Lam y st (Var x))) (Var x)) 
    `shouldBe` 
    reduce (Lam y st (Var x))

testB :: Spec
testB = it "(\\x y -> y x) y x = x y" $ 
    reduce (App (App (Lam x st (Lam y st (App (Var y) (Var x)))) (Var y)) (Var x)) 
    `shouldBe` 
    reduce (App (Var x) (Var y))

testC :: Spec
testC = it "(\\x -> \\x -> x) y = \\x -> x" $ 
    reduce (App (Lam x st (Lam x st (Var x))) (Var y)) 
    `shouldBe` 
    reduce (Lam x st (Var x))

testD :: Spec
testD = it "(\\x y z -> \\x y -> y x z) z x y y x = x y y" $ 
    reduce (App (App (App (App (App (Lam x st (Lam y st (Lam z st (Lam x st (Lam y st (App (App (Var y) (Var x)) (Var z))))))) (Var z)) (Var x)) (Var y)) (Var y)) (Var x)) 
    `shouldBe` 
    reduce (App (App (Var x) (Var y)) (Var y))

testE :: Spec
testE = it "(\\x y z -> (\\x -> y z) x) (\\x -> x) (\\x y -> x) (\\x y -> x y) = \\y x -> x" $
    reduce (App (App (App (Lam x st (Lam y st (Lam z st (App (Lam x st (App (Var y) (Var z))) (Var x))))) (Lam x st (Var x))) (Lam x st (Lam y st (Var x)))) (Lam x st (Lam y st (App (Var x) (Var y))))) 
    `shouldBe`
    reduce (Lam y st (Lam x st (Var x)))