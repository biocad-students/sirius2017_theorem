import Lib
import Type
import Reduce

main :: IO ()
main = do
    let t1 = V "x"
        t2 = V "y"
        t3 = Lam t1 (Lam t2 (App (Lam t1 $ Var t1) $ Var t2))
        t4 = App t3 (App (Var t2) (Var t1))
    putStr . showTerm . reduce $ t4