
{-
A. Design a (recursive) datatype Expr that represents expressions of the above kind.
You may represent integer numbers by floating point numbers; it is not necessary to have 
two different constructor functions for this. Your data type should be designed to make
 it easy to add more functions and more binary operators to the language.
-}
data Expr = Num Float
            | Var x
            | Add Expr Expr
            | Mul Expr Expr
            | Cos Expr
            | Sin Expr

--B
showExpr :: Expr -> String
showExpr = undefined

