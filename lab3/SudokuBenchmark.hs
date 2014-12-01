module SudokuBenchmark where
import Sudoku
import Control.Monad
import Data.Functor
import Data.Text (pack, splitOn)
import Control.Exception
import System.CPUTime
import System.Directory
import System.FilePath
import Data.List

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
  putStrLn "Starting to solve sudokus..."
  putStrLn ""
  results <- mapM timeSudoku $ zip sudokus sudokuFiles
  let solveResults = map fst results
  let slowSolveResults = map snd results
  let solveTotal = sum solveResults
  let slowSolveTotal = sum slowSolveResults
  let solveAverage = solveTotal / genericLength solveResults
  let slowSolveAverage = slowSolveTotal / genericLength slowSolveResults
  putStrLn $ "Done!"
  putStrLn $ "Total of solve:" ++ show solveTotal ++ "."
  putStrLn $ "Total of slowSolve:" ++ show slowSolveTotal ++ "."
  putStrLn $ "Average of solve:" ++ show solveAverage ++ "."
  putStrLn $ "Average of slowSolve:" ++ show slowSolveAverage ++ "."

timeSudoku (s,fname) = do
  let name = (last (splitOn (pack "/") (pack fname)))
  putStrLn $ "-- Solving "++ show name ++ " --"
  secs <- time $ solve s`seq` return ()
  slowSecs <- time $ slowSolve s`seq` return ()
  let s' = solve s
  putStrLn "Finished with the following result from solve:"
  case s' of
    Just sud -> printSudoku sud
    Nothing  -> putStrLn "(No solution)"
  putStrLn $ show secs ++ " s with solve."
  putStrLn $ show slowSecs ++ " s with slowSolve."
  putStrLn ""
  return (secs, slowSecs)
