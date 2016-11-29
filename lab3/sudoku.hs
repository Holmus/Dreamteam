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
    [ [Just 3, Just 6, Nothing,Nothing,Just 7, Just 2, Nothing,Nothing,Nothing]
    , [Nothing,Just 5, Nothing,Nothing,Nothing,Nothing,Just 1, Just 8, Nothing]
    , [Nothing,Nothing,Just 9, Just 2, Nothing,Just 4, Just 7, Nothing,Nothing]
    , [Nothing,Nothing,Nothing,Nothing,Just 1, Just 3, Nothing,Just 2, Just 8]
    , [Just 4, Nothing,Nothing,Just 5, Nothing,Just 2, Nothing,Nothing,Just 9]
    , [Just 2, Just 7, Nothing,Just 4, Just 6, Nothing,Nothing,Nothing,Nothing]
    , [Nothing,Nothing,Just 5, Just 3, Nothing,Just 8, Just 9, Nothing,Nothing]
    , [Nothing,Just 8, Just 3, Nothing,Nothing,Nothing,Nothing,Just 6, Nothing]
    , [Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]
    ]

-- Creates an empty sudoku board consisting of a 9x9 table "filled" with Nothing
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate 9 (replicate 9 Nothing))

-- Given a sudoku, verifies that it's a 9 by 9 sudoku. With only allowed values (Nothing or Just 1-9)
isSudoku :: Sudoku -> Bool
isSudoku s = length ([(Just x) | (Just x) <- oneArray, x < 10 && x > 0]
                    ++ [y | y <- oneArray, y == Nothing])
                    == 81 && length (getRows s) == 9
             where oneArray = concat (rows s)

-- For the sudoku to be solved it needs to be a legit sudoku, that is, it can contain no "Nothing"-elements and has to consist of 1-9's solely.
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

-- ?
printSudoku :: Sudoku -> IO ()
printSudoku s | (rows s) == [] = return ()
              | otherwise = putStr (convertToString (map (map printFormat) (rows s)))

-- Given a filepath creates a sudoku from the string found in the file
readSudoku :: FilePath -> IO Sudoku
readSudoku path = do string <- readFile path
                     return (createSudoku string)

-- Given a char . or 1-9 returns it as a Maybe Int, i.e Just 1-9 or Nothing
charToMaybe :: Char -> Maybe Int
charToMaybe c | c == '.' = Nothing
              | isDigit c = (Just (digitToInt c)) 

-- Given a string creates the corresponding sudoku if the string is using the desired notation (1-9, .)
createSudoku :: String -> Sudoku
createSudoku s | isSudoku sudoku = sudoku
               | otherwise = error "The provided file is not a sudoku"
               where sudoku =  Sudoku ([[charToMaybe i | i <- x]| x <- words s])

-- Generates a cell, with 10% prob, of a Just int and 90% prob of Nothing. Utilizes randomJust
cell :: Gen (Maybe Int)
cell =  frequency [(1,randomJust),(9,elements [Nothing])]

-- Generates a just int between just 1-9
randomJust :: Gen (Maybe Int)
randomJust = elements [Just i | i <- [1..9]]

instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

-- Utilizes isSudoku to check whether the sudoku solely consists of allowed values
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku s = isSudoku s

-- Datatype consisting of an array of maybe ints, which we intend to use to represent one square, col or row of length 9
type Block = [Maybe Int]  

-- Verifies that the block provided is of length 9, consists of no Nothings and is solely unique values. If so returns true
isOkayBlock :: Block -> Bool
isOkayBlock block | length block == 9 = length (nub numBlock) == length numBlock
                    where numBlock = filter (\a -> if a == Nothing then False else True) block

-- Creates blocks from a provided sudoku, if the sudoku consists of disallowed characters: returns [].
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

-- Given a sudoku and two indexes, returns a 3x3 square at the provided position
getSquare :: [Block] -> Int -> Int -> Block
getSquare rows x y  = concat [ take 3 (drop x row) | row <- take 3 (drop y rows)]

-- 
prop_blocks_amountOfCells :: Sudoku -> Bool
prop_blocks_amountOfCells (Sudoku rows) = length (blocks (Sudoku rows)) == 27
                                        && all (\a -> if length a == 9 then True else False) rows

isOkay :: Sudoku -> Bool
isOkay s = and (map isOkayBlock (rows s))


--E

type Pos = (Int,Int)

--E1

blanks :: Sudoku -> [Pos]
blanks s = [ (x,y)| x <- [0..8], y <- [0..8], isNothing ( ((rows s)!!x) !!y )]

prop_blanks_allBlank :: Sudoku -> Bool
prop_blanks_allBlank (Sudoku rows) = all (\(x,y) -> rows!!x!!y == Nothing) (blanks (Sudoku rows)) 

--E2
--Also write (a) propert(y/ies) that state(s) the expected properties of this function. Think about what can go wrong! ????
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) [] _          = [] 
(!!=) (x:xs) (i,el) | i < 0 || (length (x:xs) - 1) < i =
                      error "Invalid index"
                    | i == 0 = el:xs
                    | otherwise = x:((!!=) xs (i-1,el))


--Write prop replace


--E3

update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update (Sudoku rows) (row,col) el = Sudoku ((!!=) rows (row, ((!!=) (rows!!row) (col,el))))

{-
prop_update_correctVal :: Sudoku -> (Positive Int, Positive Int) -> Maybe Int -> Bool
prop_update_correctVal s (row,col) el = (rows (update s (row,col) el))!!row!!col == el
-}
--E4

candidates :: Sudoku -> Pos -> [Int]
candidates sud (x,y) | isSudoku sud = intersect
                        (intersect (checkBlock row y) (checkBlock col x))
                        (checkBlock square arPos) 
                     | otherwise = []  
                       where block = blocks sud
                             sqPos = getSquarePos (x,y)
                             arPos = getArrPos (x,y)
                             row   = block!!x
                             col   = block!!(9 + y)
                             square= block!!(18 + sqPos) 


--Finds which of the 9x9 block the x,y position belongs to. 
getSquarePos :: Pos -> Int
getSquarePos (x,y) = (x`div`3)*3 + y`div`3

--Find which position in the array of a 3x3 block a position has
getArrPos :: Pos -> Int
getArrPos (x,y) = x `mod` 3 * 3 + y `mod` 3


checkBlock :: [Maybe Int] -> Int -> [Int]
checkBlock arr pos = [ y | y <- [1..9], isOkayBlock ((!!=) arr (pos,Just y))]

-- F1

solve :: Sudoku -> Maybe Sudoku
solve s | isSudoku s && isOkay s = solve' s (blanks s)
        | otherwise = Nothing

solve' :: Sudoku -> [Pos] -> Maybe Sudoku
solve' s [] = Just s        
solve' s (fBlank:rBlank) = listToMaybe $ catMaybes 
                           [ solve' x rBlank | x <- 
                            [update s fBlank (Just cand) | cand <- candidates s fBlank]
                           ]  
        
-- F2        
readAndSolve :: FilePath -> IO ()
readAndSolve path = do 
                        s <- readSudoku path
                        let temp = solve s
                        if (isNothing temp)
                           then putStr "(No Solution) \n"
                        else printSudoku (fromJust (temp))

-- F3
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf s1 s2 = isSolved s1 && isSolutionOf' s1 s2

isSolutionOf' :: Sudoku -> Sudoku -> Bool
isSolutionOf' (Sudoku s1) (Sudoku s2) = all (\(x,y) -> if s1!!x!!y == s2!!x!!y then True else False) $
                                  [(x,y) | x <- [0..8], y <- [0..8], notElem (x,y) $ blanks (Sudoku s2)]

-- F4 

prop_SolveSound :: Sudoku -> Property
prop_SolveSound s = not (isNothing (solution)) ==>  property $ isSolved $ fromJust $ solution
                    where solution = solve s 


fewerChecks prop = quickCheckWith stdArgs{ maxSuccess = 30 } prop

