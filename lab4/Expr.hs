
{-
A. Design a (recursive) datatype Expr that represents expressions of the above kind.
You may represent integer numbers by floating point numbers; it is not necessary to have 
two different constructor functions for this. Your data type should be designed to make
 it easy to add more functions and more binary operators to the language.
-}
data Expr = Num Float
            | Add Expr Expr
            | Mul Expr Expr
            | Cos Expr
            | Sin Expr

--B
showExpr :: Expr -> String
showExpr (Num f) = show f
showExpr (Add e1 e2) =  showExpr e1 ++ "+" ++ showExpr e2 
showExpr (Mul (Num f1) (Num f2)) = showExpr (Num f1) ++ "*" ++ showExpr (Num f2)
showExpr (Mul e1 (Num f)) = "(" ++ showExpr e1 ++ ")*" ++ showExpr (Num f)
showExpr (Mul (Num f) e2 ) =  showExpr (Num f) ++ "*(" ++ showExpr e2 ++ ")"
showExpr (Mul e1 e2) = "(" ++ showExpr e1 ++ ")*(" ++  showExpr e2 ++ ")"

