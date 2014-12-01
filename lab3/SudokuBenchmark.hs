module SudokuBenchmark where
import Sudoku
import Control.Monad
import Data.Functor
import Data.Text (pack, splitOn)
import Control.Exception
import System.CPUTime
import System.Directory
import System.FilePath

{-

Quick and dirty tool for benchmarking our two solving algorithms

Please do not consider this code for style points ;)

-}

time a = do
  start <- getCPUTime
  v <- a
  end <- getCPUTime
  let diff = fromIntegral (end - start) / (10^12)
  return diff

main = do
  sudokuFiles <- getDirectoryContents "sudokus" >>=
    mapM (canonicalizePath . ("sudokus" </>)) >>=
    filterM (fmap not . doesDirectoryExist)
  sudokus <- mapM readSudoku sudokuFiles
  putStrLn "Starting..."
  results <- mapM timeSudoku $ zip sudokus sudokuFiles
  let solveTotal = sum $ map fst results
  let slowSolveTotal = sum $ map snd results
  putStrLn $ "Done! A total of " ++ show solveTotal ++ " s with solve and "
    ++ show slowSolveTotal ++ "s with slowSolve."

timeSudoku (s,fname) = do
  secs <- time $ solve s`seq` return ()
  slowSecs <- time $ slowSolve s`seq` return ()
  putStrLn $ "Solved "++ show (last (splitOn (pack "/") (pack fname)))++" with solve in "
    ++ show secs ++ " s and slowSolve in " ++ show slowSecs ++ " s."
  return (secs, slowSecs)
