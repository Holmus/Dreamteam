module Expr where  
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
readExpr st | isNothing expr = Nothing
            | otherwise = Just $ fst $ fromJust expr 
  where expr = parse expression (rmWSpace st)

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
             return $ Num $ read $ (show i) ++ "." ++ (show j)

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



--F
simMul,simAdd :: Expr -> Expr -> Expr
simplify,simFun,simSin,simCos,simOper :: Expr -> Expr

simplify o@(Oper a b c) = (simOper o)
simplify f@(Fun _ _)    = (simFun f)
simplify (Num f)        = (Num f)
simplify (Var x)        = (Var x)

simOper (Oper (Bin _ t _) e e1) | t == "*" = (simMul (simplify e) (simplify e1))
                                | otherwise = (simAdd (simplify e) (simplify e1))

simFun (Fun (Un _ t _) e) | t == "sin" = (simSin (simplify e))
                          | otherwise = (simCos (simplify e))

simCos (Num 0) = (Num 1)
simCos e       = (Fun cos' e)

simSin (Num 0) = (Num 0)
simSin e       = (Fun sin' e)

simMul (Num 0) _       = Num 0
simMul _ (Num 0)       = Num 0
simMul (Num 1) e       = e
simMul e (Num 1)       = e
simMul (Num x) (Num y) = (Num (x*y))
simMul e e1            = (Oper mul e e1)

simAdd (Num 0) e       = e
simAdd e (Num 0)       = e 
simAdd (Var x) (Var y) = (Oper mul (Num 2) (Var x))
simAdd (Num x) (Num y) = (Num (x+y))
simAdd e e1            = (Oper add e e1)

--G
differentiate :: Expr -> Expr
differentiate (Num i)        = (Num 0)
differentiate (Var x)        = (Num 1)
differentiate f@(Fun _ _)    = simplify (diffFun f)
differentiate o@(Oper _ _ _) = simplify (diffOper o)

diffOper (Oper (Bin _ t _) e e1) 
    | t == "+"  = (Oper add (differentiate e) (differentiate e1))
    | otherwise = (Oper add 
                    (Oper mul (differentiate e) e1 ) 
                    (Oper mul e (differentiate  e1))
                   ) --Kedjeregeln

diffFun (Fun (Un _ t _) e) 
    | t == "sin" = (Oper mul (Fun cos' e) (differentiate e))
    | otherwise = (Oper mul 
                    (Oper mul (Num (-1)) (Fun sin' e)) 
                    (differentiate e)
                  ) 

