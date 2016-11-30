import Data.Maybe
import Data.Char
import Test.QuickCheck
import Data.List


data Sudoku = Sudoku { rows :: [[Maybe Int]] }
                deriving Show

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

--A1  Creates an empty sudoku consisting of a 9x9 table "filled" with Nothing
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate 9 (replicate 9 Nothing))

--A2 Given a sudoku, verifies that it's a 9 by 9 sudoku. With only allowed values (Nothing or Just 1-9)
isSudoku :: Sudoku -> Bool
isSudoku s = length ([(Just x) | (Just x) <- oneArray, x < 10 && x > 0]
                    ++ [y | y <- oneArray, y == Nothing])
                    == 81 && length (getRows s) == 9
             where oneArray = concat (rows s)

--A3 For the sudoku to be solved it needs to be a legit sudoku, that is, it can contain no "Nothing"-elements and has to consist of 1-9's solely.
isSolved :: Sudoku -> Bool
isSolved s = isSudoku s && notElem Nothing (concat (rows s))

-- Given a maybe int, it returns it as a char. That is a Just 1-9, returns 1-9 and Nothing returns '.'
printFormat :: Maybe Int -> Char
printFormat (Just x) = intToDigit x
printFormat Nothing = '.'

-- Given an array of chars it combines them together as a string, separating each character with the new line
convertToString :: [[Char]] -> String
convertToString (x:[]) = x ++ "\n"
convertToString (x:xs) = x ++ "\n" ++ convertToString xs

--B1 Given a sudoku, creates instructions to print the sudoku on the screen, Nothing is represented by '.'
printSudoku :: Sudoku -> IO ()
printSudoku s | (rows s) == [] = return ()
              | otherwise = putStr (convertToString (map (map printFormat) (rows s)))

--B2 Given a filepath creates instructions that reads a sudoku from the file
readSudoku :: FilePath -> IO Sudoku
readSudoku path = do string <- readFile path
                     return (createSudoku string)

-- Given a char '.'' or an int 1-9 returns it as a Maybe Int, i.e Just 1-9 or Nothing
charToMaybe :: Char -> Maybe Int
charToMaybe c | c == '.' = Nothing
              | isDigit c = (Just (digitToInt c)) 

-- Given a string creates the corresponding sudoku if the string is using the desired notation (1-9, .).
-- Utilizes charToMaybe
createSudoku :: String -> Sudoku
createSudoku s | isSudoku sudoku = sudoku
               | otherwise = error "The provided file is not a sudoku"
               where sudoku =  Sudoku ([[charToMaybe i | i <- x]| x <- words s])

--C1 Generates a cell, with 10% prob of a Just int and 90% prob of Nothing. Utilizes randomJust
cell :: Gen (Maybe Int)
cell =  frequency [(1,randomJust),(9,elements [Nothing])]

-- Generates a Just int between Just 1-9
randomJust :: Gen (Maybe Int)
randomJust = elements [Just i | i <- [1..9]]


--C2
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

--C3 Property for the isSudoku-function to check whether the sudoku solely consists of allowed values
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku s = isSudoku s

type Block = [Maybe Int]  

--D1 Verifies that the block provided is of length 9, consists of unique values with disregard to nothing.
-- Returns true if those conditions are met
isOkayBlock :: Block -> Bool
isOkayBlock block | length block == 9 = length (nub numBlock) == length numBlock
  where numBlock = filter (\a -> if a == Nothing then False else True) block

--D2 Creates an array of blocks from a provided sudoku, if the sudoku consists of disallowed characters: returns [].
-- Utilizes getRows, getColumns and getSquare
blocks :: Sudoku -> [Block]
blocks s | isSudoku s = (getRows s) ++ (getColums s) ++ squares
         | otherwise  = [] 
          where squares = [getSquare (rows s) x y | y <- [0,3,6], x <- [0,3,6]]

-- Given a sudoku, returns all the rows as an array of blocks.
getRows :: Sudoku -> [Block]
getRows s = rows s

-- Given a sudoku, returns all the columns as an array of blocks.
getColums :: Sudoku -> [Block]
getColums s = transpose (rows s)

-- Given a sudoku and two indexes, returns a 3x3 square at the provided position, where the indexes indicate the top-left corner.
getSquare :: [Block] -> Int -> Int -> Block
getSquare rows x y  = concat [ take 3 (drop x row) | row <- take 3 (drop y rows)]

-- Verifies that the number of cells are correct, that is, the return value of the blocks function should consist of 27x9 elements. 
prop_blocks_amountOfCells :: Sudoku -> Bool
prop_blocks_amountOfCells (Sudoku rows) = length (blocks (Sudoku rows)) == 27
                                        && all (\a -> if length a == 9 then True else False) rows

--D3 Checks through all rows and verifies that they are okay blocks, thus the whole sudoku is okay.
isOkay :: Sudoku -> Bool
isOkay s = and (map isOkayBlock (blocks s))
