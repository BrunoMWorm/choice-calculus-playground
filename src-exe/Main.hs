module Main (main) where

import CC.ChoiceCalculus (Dim, V (Chc, Dim, Obj), semantics)
import Examples.VExpr (Expr (Lit, Add, VExpr), eval, evalEA, evolution)
import CC.Edit (hoist)
import Examples.Menu (menu, Food)
import Data.Data (Data)
import qualified Examples.VList

e1 :: Expr
e1 = Add (Lit 1) (Lit 2)

e2 :: Expr
e2 = Add (Lit 3) (Lit 4)

e3 :: Expr
e3 =
  VExpr
    ( Dim
        "Expr"
        ["e1", "e2"]
        (Chc "Expr" [Obj e1, Obj e2])
    )

e4 :: Expr
e4 =
  VExpr
    ( Dim
        "Op"
        ["Add1", "Add2"]
        (Chc "Op" [Obj (Add e3 (Lit 1)), Obj (Add e3 (Lit 2))])
    )


main :: IO ()
main = do
  putStrLn "Expressions"
  let e = Add e1 e2 in print $ evalEA (Lit 0) [] 0 e
