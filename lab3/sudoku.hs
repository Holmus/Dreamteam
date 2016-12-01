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
isSudoku s = all (==9) (map length (rows s)) 
          && all (==9) (map length (transpose (rows s))) 
          && all legitVal (concat (rows s))
          where legitVal (Just x) = if x > 0 && x < 10 then True else False
                legitVal Nothing  = True 



--A3 For the sudoku to be solved it needs to be a legit sudoku, that is, it can contain no "Nothing"-elements and has to consist of 1-9's solely.
isSolved :: Sudoku -> Bool
isSolved s = notElem Nothing (concat (rows s))

-- Given a maybe int, it returns it as a char. That is a Just 1-9, returns 1-9 and Nothing returns '.'
printFormat :: Maybe Int -> Char
printFormat (Just x) = intToDigit x
printFormat Nothing = '.'

--B1 Given a sudoku, creates instructions to print the sudoku on the screen, Nothing is represented by '.'
printSudoku :: Sudoku -> IO ()
printSudoku s | (rows s) == [] = return ()
              | otherwise = putStr (unlines (map (map printFormat) (rows s)))

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
        where randomJust = elements [Just i | i <- [1..9]]


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
isOkayBlock block = length (nub numBlock) == length numBlock
   where numBlock = filter isJust block

--D2 Creates an array of blocks from a provided sudoku, if the sudoku consists of disallowed characters: returns [].
-- Utilizes getRows, getColumns and getSquare
blocks :: Sudoku -> [Block]
blocks s | isSudoku s = (rows s) ++ (transpose (rows s)) ++ squares
         | otherwise  = [] 
          where squares = [getSquare (rows s) x y | y <- [0,3,6], x <- [0,3,6]]

-- Given a sudoku and two indexes, returns a 3x3 square at the provided position, where the indexes indicate the top-left corner.
getSquare :: [Block] -> Int -> Int -> Block
getSquare rows x y  = concat [ take 3 (drop x row) | row <- take 3 (drop y rows)]

-- Verifies that the number of cells are correct, that is, the return value of the blocks function should consist of 27x9 elements. 
prop_blocks_amountOfCells :: Sudoku -> Bool
prop_blocks_amountOfCells (Sudoku rows) = length (blocks (Sudoku rows)) == 27
                                        && all (\a -> if length a == 9 then True else False) rows

--D3 Checks through all rows and verifies that they are okay blocks, thus the whole sudoku is okay.
isOkay :: Sudoku -> Bool
isOkay s = all isOkayBlock (blocks s)


--E, tuple of ints, represents a position.
type Pos = (Int,Int)

-- E1

-- Finds and returns all Nothings as an array of Pos's,
blanks :: Sudoku -> [Pos]
blanks s = [ (x,y)| x <- [0..8], y <- [0..8], isNothing ( ((rows s)!!x) !!y )]

-- For a provided sudoku, finds all nothing positions and verifies for each of those that they are indeed, nothing.
prop_blanks_allBlank :: Sudoku -> Bool
prop_blanks_allBlank (Sudoku rows) = all (\(x,y) -> rows!!x!!y == Nothing) (blanks (Sudoku rows)) 

-- E2
-- Given a list and a tuple containing an index and a element,
-- replace the element at the index with the elemeent provided.
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) [] _          = [] 
(!!=) (x:xs) (i,el) | i < 0 || (length (x:xs) - 1) < i =
                      error "Invalid index"
                    | i == 0 = el:xs
                    | otherwise = x:((!!=) xs (i-1,el))


-- Creates valid test input for prop_replace_*, doesn't work for empty list.  
rInt :: [a] -> Gen Int
rInt arr = do
              x <- elements [0..((length arr)-1)]
              return x


-- Property to verify that the length of the list is unchanged, that is, the element has been replaced, not solely the new one inserted nor the old one deleted.
prop_replace_length' :: [a] -> a -> Property
prop_replace_length' arr el = forAll (rInt arr) test
                              where test i = length arr == length (arr !!= (i,el))

-- Property to verify that the element is inserted and that the rest of the list remains unchanged.
prop_replace_correct' :: Eq a => [a] -> a -> Property  
prop_replace_correct' [] el  = property ((!!=) [] (0,el) == [])  
prop_replace_correct' arr el = forAll (rInt arr) test
                                  where test i = (!!=) arr (i,el)!!i == el && and [True | j <- [0..((length arr)-1)],not (j == i)]

--E3

-- Function to update the provided sudoku at a provided position with a provided element, utilizes (!!=)
update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update (Sudoku rows) (row,col) el = Sudoku ((!!=) rows (row, ((!!=) (rows!!row) (col,el))))

-- Creates valid test input for prop_update_correctval.
rPos :: Gen Pos
rPos = do
          x <- elements [0..8]
          y <- elements [0..8]
          return (x,y)

-- Verifies that the element at the index provided is updated and correct.      
prop_update_correctVal :: Sudoku -> Maybe Int -> Property
prop_update_correctVal s el = forAll rPos update'
  where update' (x,y) = upArr(x,y)!!x!!y == el 
        upArr   (x,y) = (rows (update s (x,y) el))
--E4
-- Returns all ints that could fill in the position provided in the sudoku provided.
-- For values already filled in, returns a list consisting of the current one as well as possible replacements
candidates :: Sudoku -> Pos -> [Int]
candidates sud (x,y) | isSudoku sud = intersect
                        (intersect (checkBlock row y) (checkBlock col x))
                        (checkBlock square arPos) 
                     | otherwise   = []  
                       where block = blocks sud
                             sqPos = getSquarePos (x,y)
                             arPos = getArrPos (x,y)
                             row   = block!!x
                             col   = block!!(9 + y)
                             square= block!!(18 + sqPos) 
-- Creates valid test input for prop_candidates, it randomizes positions among those that are blank for the provided sudoku.
rBlank :: Sudoku -> Gen Pos
rBlank s = do
              (x,y) <- elements (blanks s)
              return (x,y)

-- Checks whether the candidate functions findings are actual candidates.
prop_candidates :: Sudoku -> Property
prop_candidates sud = isOkay sud ==> forAll (rBlank sud) candidates'
  where allOkay arr     = and $ map isOkay arr
        candidates' pos = allOkay [update sud pos (Just c) | c <- candidates sud pos]
        
--Finds which of the 9x9 block the x,y position belongs to. 
getSquarePos :: Pos -> Int
getSquarePos (x,y) = (x`div`3)*3 + y`div`3

--Find which position in the array of a 3x3 block a position has
getArrPos :: Pos -> Int
getArrPos (x,y) = x `mod` 3 * 3 + y `mod` 3

-- For a provided list of maybe ints and an int checks where in the block that int could be inserted.
checkBlock :: [Maybe Int] -> Int -> [Int]
checkBlock arr pos = [ y | y <- [1..9], isOkayBlock ((!!=) arr (pos,Just y))]

-- F1
-- Checks whether the sudoku provided can actually be solved, then utlizes solve' to actually compute the solution.
solve :: Sudoku -> Maybe Sudoku
solve s | isSudoku s && isOkay s = solve' s (blanks s)
        | otherwise = Nothing

-- Solves the sudoku provided by going over the blanks provided and trying out different candidates.
solve' :: Sudoku -> [Pos] -> Maybe Sudoku
solve' s [] = Just s        
solve' s (fBlank:rBlank) = listToMaybe $ catMaybes 
                           [ solve' x rBlank | x <- 
                            [update s fBlank (Just cand) | cand <- candidates s fBlank]
                           ]  
        
-- F2      
-- Reads the file on the path provided and creates instructions to solve it and prints the answer.  
readAndSolve :: FilePath -> IO ()
readAndSolve path = do 
                        s <- readSudoku path
                        let temp = solve s
                        if (isNothing temp)
                           then putStr "(No Solution) \n"
                        else printSudoku (fromJust (temp))

-- F3
-- Checks whether the 1st sudoku provided is solved, then by utilizing the helper-function if the 2nd sudoku is a "sub-sudoku" of the 1st.
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf s1 s2 = isSolved s1 && isSolutionOf' s1 s2

-- Helper function which checks if the 2nd sudoku is a sub-sudoku of the first
isSolutionOf' :: Sudoku -> Sudoku -> Bool
isSolutionOf' (Sudoku s1) (Sudoku s2) = 
  all (\(x,y) -> if s1!!x!!y == s2!!x!!y then True else False) $
  [(x,y) | x <- [0..8], y <- [0..8], notElem (x,y) $ blanks (Sudoku s2)]

-- F4 
-- Property to verify that all sudokus solved are actually solved and valid by the original sudoku provided.
prop_SolveSound :: Sudoku -> Property
prop_SolveSound s = not (isNothing (solution)) ==>  property $ isSolved $
                    fromJust $ solution
                    where solution = solve s 


fewerChecks prop = quickCheckWith stdArgs{ maxSuccess = 30 } prop

