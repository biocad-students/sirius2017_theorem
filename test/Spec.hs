module Main where

import Lawer
import LawerHL
import Test.Hspec
import Std
import Data.Text (pack)
import Prelude hiding (not, succ, and, or)
import Control.Monad.Trans.Except (runExcept)

main :: IO ()
main = hspec $ do
    testsReduceAndTypes 
    testEncoding

testEncoding :: SpecWith ()
testEncoding = describe "Test Encoding" $ do
    testAlgebraicEncoding
    testInductiveEncoding 

testAlgebraicEncoding :: Spec
testAlgebraicEncoding = do
    it (show . constructionToTerm $ algBool) $ bool `shouldBe` bool
    it (show . constructionToTerm $ algList) $ 1 `shouldBe` 1
    it (show . constructionToTerm $ algNat) $ true `shouldBe` true
    it (show . constructionToTerm $ algPair) $ true `shouldBe` true
    
testInductiveEncoding :: Spec 
testInductiveEncoding = do 
    it (show . constructionToTerm $ indEq) $ bool `shouldBe` bool
    it (show . constructionToTerm $ indList) $ bool `shouldBe` bool

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
    it "1 + 1 = 2\n" $ reduce (plus `app` one `app` one) `shouldBe` reduce two 
    it (show . typeOf . reduce $ nat) $ reduce comm `shouldBe` reduce comm
    
    it "1 + n == n + 1" $ runExcept (typeOf (Fa n nat $ refl $$ Var n)) `shouldBe` Right (reduce $ Fa n nat $ eq $$ (plus $$ zero $$ Var n) $$ Var n) 

    -- it "2 + 4 = 4 + 2" $ reduce (eq $$ (plus $$ two $$ four) $$ (plus $$ four $$ two)) `shouldBe` reduce (refl $$ (plus $$ four $$ two))
    -- it (show $ reduce (succ $$ (plus $$ one $$ two))) $ typeOf (reduce (App (App plus two) four)) `shouldBe` typeOf (reduce (App (App plus four) two))
    -- it (show . typeOf . reduce $ comm) $ reduce comm `shouldBe` reduce comm
    -- it (show . typeOf $ reduce (commType $$ reduce (plus $$ (succ $$ one) $$ two) $$ reduce (succ $$ (plus $$ one $$ two)))) $ bool `shouldBe` bool

testBool :: Spec
testBool = do 
    it "not false = true"           $ reduce (not $$ false)                 `shouldBe` true
    it "true and false = false"     $ reduce (and $$ true $$ false)         `shouldBe` false
    it "true or false = true"       $ reduce (or $$ true $$ false)          `shouldBe` true
    it "not false =types= true"     $ typeOf (reduce (not $$ false))        `shouldBe` typeOf true
    it "true and false =t= false"   $ typeOf(reduce (and $$ true $$ false)) `shouldBe` typeOf false
    it "true or false =t= true\n"   $ typeOf(reduce (or $$ true $$ false))  `shouldBe` typeOf true

    it "bool = bool"                $ runExcept (typeOf (refl $$ bool)) `shouldBe` Right (reduce $ eq $$ bool $$ bool)
    -- it ("bool = " ++ show bool) $ bool `shouldBe` bool
    -- it ("true = " ++ show true) $ bool `shouldBe` bool
    -- it ("false = " ++ show false) $ bool `shouldBe` bool
    -- it ("not = " ++ show not) $ bool `shouldBe` bool
    -- it ("and = " ++ show and) $ bool `shouldBe` bool
    -- it ("or = " ++ show or ++ "\n") $ bool `shouldBe` bool

    -- it "true == true" $ reduce (eq $$ true $$ true) `shouldBe` reduce (refl $$ true)

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