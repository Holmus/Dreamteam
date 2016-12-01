
{-
A. Design a (recursive) datatype Expr that represents expressions of the above kind.
You may represent integer numbers by floating point numbers; it is not necessary to have 
two different constructor functions for this. Your data type should be designed to make
 it easy to add more functions and more binary operators to the language.
-}
data Expr = Num Double
            | Var Char
            | Oper Bin Expr Expr
            | Fun Un Expr

data Bin = Bin {binFun :: (Double->Double->Double), binText :: String, binPrec :: Int}


data Un = Un {fun :: (Double->Double), text :: String, prec :: Int}

add = Bin (+) " + " 2
mul = Bin (*) " * " 3
sin' = Un sin "Sin" 4
cos' = Un cos "Cos" 4

--B
{-

showExpr :: Expr -> String
showExpr (Num f)       = show f
showExpr (Var x)       = "x" 
showExpr (Add e1 e2)   = showExpr e1 ++ "+" ++ showExpr e2 
showExpr (Mul e1 e2)   = showMul (Mul e1 e2)
showExpr (Cos e)       = showCos (Cos e)
showExpr (Sin e)       = showSin (Sin e)


showMul :: Expr -> String
showMul (Mul (Add e1 e2) (Add e3 e4)) = "(" ++ showExpr (Add e1 e2) ++ ")*(" ++ showExpr (Add e3 e4) ++ ")"
showMul (Mul e1 (Add e2 e3)) = showExpr (Add e1 e2) ++ "*(" ++ showExpr e3 ++ ")"
showMul (Mul (Add e1 e2) e3) = "(" ++ showExpr (Add e1 e2) ++ ")*"  ++ showExpr e3
showMul (Mul e1 e2) = showExpr e1 ++ "*" ++ showExpr e2

showCos :: Expr -> String 
showCos (Cos (Num x)) = "Cos" ++ show x
showCos (Cos (Var x)) = "Cos x"
showCos (Cos e) = "Cos (" ++ showExpr e ++ ")"

showSin :: Expr -> String
showSin (Sin (Num x)) = "Sin" ++ show x
showSin (Sin (Var x)) = "Sin x"
showSin (Sin e)       = "Sin (" ++ showExpr e ++ ")"

-}