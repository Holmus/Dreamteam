module ExprQC where
import Test.QuickCheck
import Expr


instance Arbitrary Expr where
  arbitrary = sized arbExpr
--Skriv om denna så att den följer rekommendationer.   


arbExpr :: Int -> Gen Expr
arbExpr size = frequency [(4,rNumVar), (2,return (Var 'x')),(2*size,rOper),(size,rFun)]
    where rNumVar = elements ([Num j | j <- [0..10]] ++ [Var 'x']) 
          rOper = do op  <- elements [add,mul]
                     op1 <- arbExpr (size `div` 2)
                     op2 <- arbExpr (size `div` 2)
                     return (Oper op op1 op2)
          rFun  = do fun <- elements [sin',cos']
                     exp <- arbExpr size
                     return (Fun fun exp)
                

 
prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr e = showExpr e == (showExpr (fromJust (readExpr (showExpr e))))