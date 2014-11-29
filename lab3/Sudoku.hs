module Sudoku where

import Test.QuickCheck
import Data.Char
import Data.List
import Data.Maybe
import Data.Ix

-------------------------------------------------------------------------

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
 deriving ( Show, Eq )

example :: Sudoku
example = Sudoku {rows =
  [[Just 3,Just 6,Nothing,Nothing,Just 7,Just 1,Just 2,Nothing,Nothing],
   [Nothing,Just 5,Nothing,Nothing,Nothing,Nothing,Just 1,Just 8,Nothing],
   [Nothing,Nothing,Just 9,Just 2,Nothing,Just 4,Just 7,Nothing,Nothing],
   [Nothing,Nothing,Nothing,Nothing,Just 1,Just 3,Nothing,Just 2,Just 8],
   [Just 4,Nothing,Nothing,Just 5,Nothing,Just 2,Nothing,Nothing,Just 9],
   [Just 2,Just 7,Nothing,Just 4,Just 6,Nothing,Nothing,Nothing,Nothing],
   [Nothing,Nothing,Just 5,Just 3,Nothing,Just 8,Just 9,Nothing,Nothing],
   [Nothing,Just 8,Just 3,Nothing,Nothing,Nothing,Nothing,Just 6,Nothing],
   [Nothing,Nothing,Just 7,Just 6,Just 9,Nothing,Nothing,Just 4,Just 3]]}

-- allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku $ replicate 9 $ replicate 9 Nothing

-- isSudoku sud checks if su is really a valid representation of a sudoku
-- puzzle (size is 9x9 and content inside Maybe is in 1..9)
isSudoku :: Sudoku -> Bool
isSudoku (Sudoku su) = length su == 9 &&
                       and [length x == 9 &&
                            all validContent (catMaybes x) | x <- su]
  where validContent = inRange (1,9)


-- isSolved sud checks if sud is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved (Sudoku rows) = [] == filter (==Nothing) (concat rows)

-------------------------------------------------------------------------

-- printSudoku sud prints a representation of the sudoku sud on the screen
-- printSudoku :: Sudoku -> IO ()
printSudoku (Sudoku rows) = putStr $ unlines $ map stringRows rows
  where
        stringRows = map elemStr
        elemStr Nothing = '.'
        elemStr (Just i) = head $ show i

-- readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku fp =
  do
    s <- readFile fp
    return $ Sudoku $ map parseRow $ lines s
  where
    parseRow = map parseElement
    parseElement c | isDigit c = Just $ digitToInt c
                   | otherwise = Nothing

-------------------------------------------------------------------------

-- cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = frequency
  [(9, return Nothing),
  (1, do n <- choose (1,9)
         return $ Just n)]

-- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

-- Property of a sudoku
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku

-------------------------------------------------------------------------

type Block = [Maybe Int]

-- Block doesn't contain the same digit twice
-- Unique count plus 'Nothing' count should be 9 + 1
-- (one 'Nothing' in the unique list in any case).
isOkayBlock :: Block -> Bool
isOkayBlock b = length (nub (Nothing:b)) == 10 - length (filter (== Nothing) b)

-- Create a list of all blocks of a sudoku (rows, cols, 3x3-fields)
blocks :: Sudoku -> [Block]
blocks (Sudoku rows) = rows ++ transpose rows ++ boxes rows

-- Create blocks from the 3x3 squares by concatenating three elements in
-- three rows at a time for all nine rows
boxes :: [Block] -> [Block]
boxes [] = []
boxes rows = boxes' (take 3 rows) ++ boxes (drop 3 rows)
  where
    boxes' ([] : _) = []
    boxes' rows = concatMap (take 3) rows : boxes' (map (drop 3) rows)

-- A sudoku should contain exactly 3x9 blocks, each block contains 9 elements
prop_SudokuCorrectBlocks :: Sudoku -> Bool
prop_SudokuCorrectBlocks sudo = length (blocks sudo) == (3*9) &&
                                and [length b == 9 | b <- blocks sudo]

-- Check that a full sudoku is valid, all blocks should be OK
isOkay :: Sudoku -> Bool
isOkay sudo = and [isOkayBlock b | b <- blocks sudo]

-------------------------------------------------------------------------

-- Following the standard (row, pos)
type Pos = (Int, Int)

-- Return a list of positions that are still blank in a sudoku
blanks :: Sudoku -> [Pos]
blanks (Sudoku rows)= [(r, p) | r <- [0..8], p <- [0..8],
                       isNothing (rows !! r !! p) ]

prop_BlanksIsBlank :: Sudoku -> Bool
prop_BlanksIsBlank (Sudoku rows) = and [isNothing (rows !! r !! p) |
                                   (r, p) <- blanks (Sudoku rows)]
-- TODO: Möjligen ett redundant test? Proppen kollar inte ifall det finns blanka
-- som inte blanks hittar.

-- Set value from the tuple at position Int in the list, like a_arr[i] = a
(!!=) :: [a] -> (Int, a) -> [a]
(!!=) [] _ = []
(!!=) (_:es) (0, a) = a:es
(!!=) (e:es) (i, a) = e:(es !!= (i-1, a))
-- Also write (a) propert(y/ies) that state(s) the expected properties of
-- this function. Think about what can go wrong!

-- Set new cell value to a specified position in a Sudoku
update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update (Sudoku rows) (r,p) v = Sudoku (rows !!= (r,
                              (rows !! r) !!= (p, v)))

prop_UpdatedSudokuIsUpdated :: Sudoku -> Bool
prop_UpdatedSudokuIsUpdated = undefined

-- Returns a list of numbers that are valid candidates at the given position
candidates :: Sudoku -> Pos -> [Int]
candidates = undefined
-- In addition, write a property that relates the function candidates with
-- the functions update, isSudoku, and isOkay. (This property can be very useful
-- to understand how to solve Sudokus!)

-------------------------------------------------------------------------

-- The actual solving function
solve :: Sudoku -> Maybe Sudoku
solve = undefined

-- Solve Sudoku from file
readAndSolve :: FilePath -> IO ()
readAndSolve = undefined
