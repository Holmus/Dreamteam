import Data.Maybe
import Data.Char
import Test.QuickCheck
import Data.List


data Sudoku = Sudoku { rows :: [[Maybe Int]] }
                deriving Show

ex1 = [Just 3, Just 9, Just 10, Nothing, Just 0]
ex2 = Sudoku [
              [Just 3, Just 4, Just 5],
              [Just 3, Just 4, Just 5],
              [Just 3, Just 4, Just 5]  
             ]

example =
   Sudoku
    [ [Just 3, Just 6, Nothing,Nothing,Just 7, Just 1, Just 2, Nothing,Nothing]
    , [Nothing,Just 5, Nothing,Nothing,Nothing,Nothing,Just 1, Just 8, Nothing]
    , [Nothing,Nothing,Just 9, Just 2, Nothing,Just 4, Just 7, Nothing,Nothing]
    , [Nothing,Nothing,Nothing,Nothing,Just 1, Just 3, Nothing,Just 2, Just 8]
    , [Just 4, Nothing,Nothing,Just 5, Nothing,Just 2, Nothing,Nothing,Just 9]
    , [Just 2, Just 7, Nothing,Just 4, Just 6, Nothing,Nothing,Nothing,Nothing]
    , [Nothing,Nothing,Just 5, Just 3, Nothing,Just 8, Just 9, Nothing,Nothing]
    , [Nothing,Just 8, Just 3, Nothing,Nothing,Nothing,Nothing,Just 6, Nothing]
    , [Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]
    ]

--A
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate 9 (replicate 9 Nothing))


isSudoku :: Sudoku -> Bool
isSudoku s = length ([(Just x) | (Just x) <- oneArray, x < 10 && x > 0]
                    ++ [y | y <- oneArray, y == Nothing])
                    == 81 
             where oneArray = concat (rows s)

-- For the sudoku to be solved it needs to be a legit sudoku, 
-- then it can not contain any element of nothing. 
isSolved :: Sudoku -> Bool
isSolved s = isSolved s && notElem Nothing (concat (rows s))


--B
printFormat :: Maybe Int -> Char
printFormat (Just x) = intToDigit x
printFormat Nothing = '.'

convertToString :: [[Char]] -> String
convertToString (x:[]) = x ++ "\n"
convertToString (x:xs) = x ++ "\n" ++ convertToString xs


printSudoku :: Sudoku -> IO ()
printSudoku s | (rows s) == [] = return ()
              | otherwise = putStr (convertToString (map (map printFormat) (rows s)))

readSudoku :: FilePath -> IO Sudoku
readSudoku path = do string <- readFile path
                     return (createSudoku string)

charToMaybe :: Char -> Maybe Int
charToMaybe c | c == '.' = Nothing
              | isDigit c = (Just (digitToInt c)) 


createSudoku :: String -> Sudoku
createSudoku s | isSudoku sudoku = sudoku
               | otherwise = error "The provided file is not a sudoku"
               where sudoku =  Sudoku ([[charToMaybe i | i <- x]| x <- words s])

--C
cell :: Gen (Maybe Int)
cell =  frequency [(1,randomJust),(9,elements [Nothing])]

randomJust :: Gen (Maybe Int)
randomJust = elements [Just i | i <- [1..9]]

instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku s = isSudoku s

--D
type Block = [Maybe Int]  

isOkayBlock :: Block -> Bool
isOkayBlock block | length block == 9 = length (nub numBlock) == length numBlock
                    where numBlock = filter (\a -> if a == Nothing then False else True) block

blocks :: Sudoku -> [Block]
blocks s | isSudoku s = (getRows s) ++ (getColums s) ++ squares
          where squares = [getSquare (rows s) x y | x <- [0,3,6], y <- [0,3,6]]

getRows :: Sudoku -> [Block]
getRows s = rows s

getColums :: Sudoku -> [Block]
getColums s = transpose (rows s)

getSquare :: [Block] -> Int -> Int -> Block
getSquare rows x y  = concat [ take 3 (drop x row) | row <- take 3 (drop y rows)]

isOkay :: Sudoku -> Bool
isOkay s = and (map isOkayBlock (rows s))
