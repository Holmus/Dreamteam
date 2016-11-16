import Data.Maybe



data Sudoku = Sudoku { rows :: [[Maybe Int]] }
                deriving Show

ex1 = [Just 3, Just 9, Just 10, Nothing, Just 0]
ex2 = Sudoku [[Just 3, Just 4, Just 5],
              [Just 3, Just 4, Just 5],
              [Just 3, Just 4, Nothing]  
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
    , [Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 9]
    ]
--A1
--let a = [(Just x) | (Just x) <- ex1,  x < 10 && x > 0] ++ [y | y <- ex1, y == Nothing]

allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate 9 (replicate 9 Nothing))


isSudoku :: Sudoku -> Bool
isSudoku s = length ([(Just x) | (Just x) <- oneArray, x < 10 && x > 0]
                    ++ [y | y <- oneArray, y == Nothing])
                    == 81 
        where oneArray = concat (rows s)

-- We here assume that it's a valid sudoku if you send it in. 
isSolved :: Sudoku -> Bool
isSolved s =  notElem Nothing (concat (rows s))

