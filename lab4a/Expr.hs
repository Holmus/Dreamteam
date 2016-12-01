
{-
A. Design a (recursive) datatype Expr that represents expressions of the above kind.
You may represent integer numbers by floating point numbers; it is not necessary to have 
two different constructor functions for this. Your data type should be designed to make
 it easy to add more functions and more binary operators to the language.
-}
data Expr = Num Float
            | Var Char
            | Add Expr Expr
            | Mul Expr Expr
            | Cos Expr
            | Sin Expr

--B
showExpr :: Expr -> String
showExpr (Num x) = show x
showExpr (Var x) = "x"
showExpr (Add e1 e2) = showAdd (Add e1 e2)
showExpr (Mul e1 e2) = showMul (Mul e1 e2)
showExpr (Cos e) = showCos (Cos e)
showExpr (Sin e) = showSin (Sin e)

showAdd :: Expr -> String
showAdd = undefined

showMul :: Expr -> String
showMul (Mul (Num e1) (Num e2)) = show e1 ++ "*" ++ show e2
showMul (Mul e1 e2)            = "(" ++ showExpr e1 ++ ")*(" ++ showExpr e2 ++ ")"

showCos :: Expr -> String
showCos (Cos (Num x)) = "Cos" ++ show x
showCos (Cos (Var x)) = "Cos x"  
showCos (Cos e1) = "Cos" ++ "(" ++  showExpr e1 ++ ")"

showSin :: Expr -> String
showSin (Sin (Num x)) = "Sin" ++ show x
showSin (Sin (Var x)) = "Sin x"
showSin (Sin e1) = "Sin" ++ "(" ++  showExpr e1 ++ ")"




    
  