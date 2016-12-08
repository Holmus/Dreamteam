module Calculator where
import Expr
import Haste

points :: Expr -> Double -> (Int,Int) -> [Point]
points 