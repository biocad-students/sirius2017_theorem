
import Lib
import Type
import Reduce

main :: IO ()
main = do
    let x = V "x"
        y = V "y"
        z = V "z"
        v = Var . V
        t3 = Lam x (Lam y (App (Lam x $ Var x) $ Var y))
        t4 = App t3 (App (Var x) (Var x))
        t5 = App (Lam x (App (Var x) (Var x))) (Var y)
        tId = Lam x $ Var x
        tConst = Lam x (Lam y (Var x))
    putStr . showTerm $ t4