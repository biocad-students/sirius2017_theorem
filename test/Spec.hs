module Main where

import Reduce
import Type
import Test.Hspec
import Data.Text (pack)

main :: IO ()
main = hspec testsReduce 

testsReduce :: SpecWith ()
testsReduce = describe "Reduction" $ do
    testBool
    testA
    testB
    testC
    testD
    testE

testBool :: Spec
testBool = it "Bool" $  let a = V $ pack "a"
                            b = V $ pack "b"
                            bool = Fa a (Uni Star) (Lam noname (Var a) $ Lam noname (Var a) (Var a))
                            true = Lam a bool (Lam noname bool (Var a))
                            false = Lam noname bool (Lam b bool (Var b))
                            not = Lam a bool (App (App (Var a) false) true)
                        in reduce (App not false) `shouldBe` true

x = V $ pack "x"
y = V $ pack "y"
z = V $ pack "z"
st = Uni Star

testA :: Spec
testA = it "(\\x y -> x) x = \\y -> x" $ reduce (App (Lam x st (Lam y st (Var x))) (Var x)) `shouldBe` reduce (Lam y st (Var x))

testB :: Spec
testB = it "(\\x y -> y x) y x = x y" $ reduce (App (App (Lam x st (Lam y st (App (Var y) (Var x)))) (Var y)) (Var x)) `shouldBe` reduce (App (Var x) (Var y))

testC :: Spec
testC = it "(\\x -> \\x -> x) y = \\x -> x" $ reduce (App (Lam x st (Lam x st (Var x))) (Var y)) `shouldBe` reduce (Lam x st (Var x))

testD :: Spec
testD = it "(\\x y z -> \\x y -> y x z) z x y y x = x y y" $ 
    reduce (App (App (App (App (App (Lam x st (Lam y st (Lam z st (Lam x st (Lam y st (App (App (Var y) (Var x)) (Var z))))))) (Var z)) (Var x)) (Var y)) (Var y)) (Var x)) `shouldBe` 
    reduce (App (App (Var x) (Var y)) (Var y))

testE :: Spec
testE = it "(\\x y z -> (\\x -> y z) x) (\\x -> x) (\\x y -> x) (\\x y -> x y) = \\y x -> x" $
    reduce (App (App (App (Lam x st (Lam y st (Lam z st (App (Lam x st (App (Var y) (Var z))) (Var x))))) (Lam x st (Var x))) (Lam x st (Lam y st (Var x)))) (Lam x st (Lam y st (App (Var x) (Var y))))) `shouldBe`
    reduce (Lam y st (Lam x st (Var x)))