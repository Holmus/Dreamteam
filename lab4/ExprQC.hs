module ExprQC where
import Test.QuickCheck
import Data.Maybe
import Expr


instance Arbitrary Expr where
  arbitrary = sized arbExpr


arbExpr :: Int -> Gen Expr
arbExpr size = frequency [(5,rNumVar), (1,return (Var 'x')),(size,rOper),(size,rFun)]
    where rNumVar = elements ([Num j | j <- [0..10]] ++ [Var 'x'] ) 
          rOper = do op  <- elements [add,mul]
                     op1 <- arbExpr (size `div` 2)
                     op2 <- arbExpr (size `div` 2)
                     return (Oper op op1 op2)
          rFun  = do fun <- elements [sin',cos']
                     exp <- arbExpr size 
                     return (Fun fun exp)


prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr e = showExpr e == (showExpr (fromJust (readExpr (showExpr e))))