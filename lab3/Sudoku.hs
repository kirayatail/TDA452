module Sudoku where

import Test.QuickCheck
import Data.Char

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
printSudoku :: Sudoku -> IO ()
printSudoku (Sudoku []) = return ()
printSudoku (Sudoku (r:rows)) =
  do
     printRow r
     printSudoku (Sudoku rows)
  where printRow :: [Maybe Int] -> IO ()
        printRow [] = putStrLn ""
        printRow (x:xs) =
          do putStr $ elemStr x
             printRow xs
        elemStr :: Maybe Int -> String
        elemStr Nothing = "."
        elemStr (Just i) = show i

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
cell = undefined

-- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

-------------------------------------------------------------------------

-- A1

-- A2

-- A3

-- B1

-- B2

-- C1

-- C2

-- C3

-- D1

-- D2

-- D3

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
