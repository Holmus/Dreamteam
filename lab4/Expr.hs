import Test.QuickCheck
import Parsing
import Data.Maybe
import Data.Char
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

add = Bin (+) "+" 2
mul = Bin (*) "*" 3
min' = Bin (-) "-" 2
div' = Bin (/) "/" 3
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

rmWSpace :: String -> String
rmWSpace [] = []
rmWSpace (x:xs)| isSpace x = rmWSpace xs
               | otherwise = [x] ++ rmWSpace xs

readExpr :: String -> Maybe Expr
readExpr st | rem == "" = Just e
            | otherwise = Nothing
    where (e,rem) = fromJust (parse expression (rmWSpace st))

expression,function,var,integer',factor,double',term,expr',function',function'' :: Parser Expr

expression = expr' <|> term

expr' = do t <- term
           char '+'
           e <- expression
           return (Oper add t e)

term  = term' <|> factor
term' = do f <- factor
           char '*'
           t <- term
           return (Oper mul f t)

factor = function <|> double' <|> integer' <|> var <|>
        do char '('
           e <- expression
           char ')'
           return e

var = do char 'x' 
         return (Var 'x')

integer' = do i <- integer
              return (Num (fromIntegral i))

double' = do i <- integer
             char '.'
             j <- integer
             return (Num (read ((show i) ++ "." ++ (show j))))

integer :: Parser Integer
integer = do i <- oneOrMore digit
             return (read i)

function = function' <|> function''
                    
function' = do f <- fFun
               e <- factor
               return (Fun f e) 

function'' = do f1 <- fFun
                char '('
                e1 <- expression
                char ')'
                return (Fun f1 e1)

fFun :: Parser Un
fFun = fSin <|> fCos

fSin :: Parser Un
fSin = do char 's'
          char 'i'
          char 'n'
          return (sin')

fCos :: Parser Un 
fCos = do char 'c'
          char 'o'
          char 's'
          return (cos')         

--E

prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr e = showExpr e == (showExpr (fromJust (readExpr (showExpr e))))

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

                
                
instance Arbitrary Expr where
  arbitrary = sized arbExpr

--F

simplify :: Expr -> Expr
simplify = undefined

prop_simplify :: Expr -> Bool
prop_simplify e = (eval e 0) == (eval (simplify e) 0)

--G
differentiate :: Expr -> Expr
differentiate = undefined
