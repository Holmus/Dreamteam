


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

allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate 9 (replicate 9 Nothing))

isSudoku :: Sudoku -> Bool
isSudoku s = rowSize == colSize && rowSize > 0
        where rowSize = countRows s
              colSize = countCols s



--Takes a sudoku and checks the amount of rows
countRows :: Sudoku -> Int
countRows s = length (rows s)
                 

--Takes a soduku and checks the amount of columns in the first row
countCols :: Sudoku -> Int
countCols s | length (rows s) == 0 = 0
countCols s = length (head (rows s))

-- Not part of lab only for fun
-- Takes a soduku and check if each column has the same 
checkSameColSize :: Sudoku -> Bool
checkSameColSize s | rows s == [] = True
checkSameColSize s = checkSameColSizeRec s (countCols s)

-- Not part of lab only for fun
-- The recursive wrapper function which determines if 
-- the previous column length is the same as the current one
checkSameColSizeRec :: Sudoku -> Int -> Bool
checkSameColSizeRec s i | length (rows s) >= 1 =
                          countCols s == i 
                       && checkSameColSizeRec (Sudoku (drop 1 (rows s))) i
                        | otherwise = True

isEleNothing :: Maybe Int -> Bool
isEleNothing m | m == Nothing = True
               | otherwise = False


isSolved :: Sudoku -> Bool
isSolved s | length (rows s) == 0 = True
isSolved s = all isEleNothing (head (rows s)) 
             && isSolved (Sudoku (drop 1 (rows s)))




