module Sudoku where

import Test.QuickCheck
import Data.Char
import Data.List

-------------------------------------------------------------------------

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
 deriving ( Show, Eq )

-- allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku $ replicate 9 $ replicate 9 Nothing

-- isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku (Sudoku su) = isNine su && and [isNine x | x <- su]
  where
    isNine :: [a] -> Bool
    isNine list = length list == 9

-- isSolved sud checks if sud is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved (Sudoku rows) = isSudoku (Sudoku rows)
  && [] == filter (==Nothing) (concat rows)

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
blocks (Sudoku rows) = rows ++ transpose rows ++ fields'' rows

-- Apply fields' to three transposed rows at a time,
-- merge the result to a list of blocks.
fields :: [Block] -> [Block]
fields []   = []
fields sudo = fields' (transpose (take 3 sudo)) ++
              fields (drop 3 sudo)

-- Take a list of 9 rows with 3 elements each,
-- concatenate three rows at a time to a block and merge with remaining blocks.
fields' :: [Block] -> [Block]
fields' []     = []
fields' blocks = concat (take 3 blocks) : fields' (drop 3 blocks)

fields'' :: [Block] -> [Block]
fields'' rows = mergeRows $ concat $ transpose $ map splitRow rows
  where
    splitRow :: [a] -> [[a]]
    splitRow [] = []
    splitRow row = take 3 row : splitRow (drop 3 row)
    mergeRows [] = []
    mergeRows row = concat (take 3 row) : mergeRows (drop 3 row)

-- A sudoku should contain exactly 3x9 blocks, each block contains 9 elements
prop_SudokuCorrectBlocks :: Sudoku -> Bool
prop_SudokuCorrectBlocks sudo = length (blocks sudo) == (3*9) &&
                                and [length b == 9 | b <- blocks sudo]

-- Check that a full sudoku is valid, all blocks should be OK
isOkay :: Sudoku -> Bool
isOkay sudo = and [isOkayBlock b | b <- blocks sudo]


-- Example sudoku:
example :: Sudoku
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
