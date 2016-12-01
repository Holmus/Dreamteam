import Test.QuickCheck
--A
data Expr = Num Double
            | Var Char
            | Oper Bin Expr Expr
            | Fun Un Expr

data Bin = Bin {binFun :: (Double->Double->Double), binText :: String, binPrec :: Int}

instance Show Bin where
    show = binText

instance Show Un where
    show = unText

instance Show Expr where
        show = showExpr

data Un = Un {fun :: (Double->Double), unText :: String, unPrec :: Int}

add = Bin (+) " + " 2
mul = Bin (*) " * " 3
min' = Bin (-) " -" 2
div' = Bin (/) " / " 3
sin' = Un sin "sin" 4
cos' = Un cos "cos" 4


--B
showExpr :: Expr -> String
showExpr (Num f)                = show f
showExpr (Var x)                = "x" 
showExpr (Oper x y z)           = let p = binPrec x in showExpr' y p ++ show x ++ showExpr' z p
showExpr (Fun x o@(Oper _ _ _)) = show x ++ "(" ++ showExpr o ++ ")"
showExpr (Fun x e)              = show x ++ showExpr e


showExpr' op@(Oper b _ _) precAbove | binPrec b < precAbove = "(" ++ showExpr op ++ ")" 
                                    | otherwise = showExpr op
showExpr' x _ = show x

--C
eval :: Expr -> Double -> Double
eval (Num a) x         = a
eval (Var a) x         = x
eval (Oper op e1 e2) x = (binFun op) (eval e1 x) (eval e2 x) 
eval (Fun u e) x       = (fun u) (eval e x) 
          
--D

readExpr :: String -> Maybe Expr
readExpr = undefined

--E
-- NOT FINISHED DOESNT GENERATE CORRECT LENGTH!
arbExpr :: Int -> Gen Expr
arbExpr i | i > 1       = rbin
          | otherwise   = rNumVar
          where rNumVar = elements ([Num j | j <- [0..10]] ++ [Var 'x']) 
                rbin    = do op1 <- elements [Oper mul,Oper add]
                             op2 <- elements [Fun sin', Fun cos']
                             e1 <- arbExpr (i-1)
                             e2 <- arbExpr (i-1)
                             elements [(op1 e1 e2),(op2 e1)]

                
instance Arbitrary Expr where
  arbitrary = sized arbExpr

prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr = undefined

--F

simplify :: Expr -> Expr
simplify x@(Num double)   = x
simplify x@(Var char)     = x
simplify o@(Oper mul _ _) = simplifyMul o
simplify o@(Oper add _ _) = simplifyAdd o
simplify f@(Fun cos' _)   = simplyfyCos f
simplify f@(Fun sin' _)   = simplyfySin f

simplifyMul :: Expr -> Expr
simplifyMul (Oper mul (Num 0) _) = Num 0
simplifyMul (Oper mul _ (Num 0)) = Num 0
simplifyMul (Oper mul e (Num 1)) = e
simplifyMul (Oper mul (Num 1) e) = e
simplifyMul (Oper mul e1 e2)     = (Oper mul (simplify e1) (simplify e1))

simplifyAdd :: Expr -> Expr
simplifyAdd (Oper add (Num 0) e) = e 
simplifyAdd (Oper add e (Num 0)) = e
simplifyAdd (Oper add e1 e2)     = (Oper add (simplify e1) (simplify e2))

simplyfyCos :: Expr -> Expr
simplyfyCos (Fun cos' (Num 0)) = (Num 1)
simplyfyCos (Fun cos' e) = (Fun cos' (simplify e))

simplyfySin :: Expr -> Expr
simplyfySin (Fun sin' (Num 1)) = (Num 1)
simplyfySin (Fun sin' e) = (Fun sin' (simplify e))

prop_simplify :: Expr -> Bool
prop_simplify e = (eval e 0) == (eval (simplify e) 0)

--G
differentiate :: Expr -> Expr
differentiate = undefined
