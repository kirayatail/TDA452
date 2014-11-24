module Sudoku where

import Test.QuickCheck
import Data.Char
import Data.List
import Data.Maybe
import Data.Ix

-------------------------------------------------------------------------

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
 deriving ( Show, Eq )

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
