
--A
data Expr = Num Float
            | Add Expr Expr
            | Mul Expr Expr

--B
showExpr :: Expr -> String
showExpr (Num f) = show f
showExpr (Add e1 e2) =  showExpr e1 ++ "+" ++ showExpr e2 
showExpr (Mul (Num f1) (Num f2)) = showExpr (Num f1) ++ "*" ++ showExpr (Num f2)
showExpr (Mul e1 (Num f)) = "(" ++ showExpr e1 ++ ")*" ++ showExpr (Num f)
showExpr (Mul (Num f) e2 ) =  showExpr (Num f) ++ "*(" ++ showExpr e2 ++ ")"
showExpr (Mul e1 e2) = "(" ++ showExpr e1 ++ ")*(" ++  showExpr e2 ++ ")"

