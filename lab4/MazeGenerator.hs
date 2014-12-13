
module MazeGenerator where
import MazeLogic
import System.Random

-- Prototype for generating a maze with pluggable algorithm.
protoGenerateMaze :: (StdGen -> Int -> Int -> Maze) -> Int -> Int -> IO ()
protoGenerateMaze f w h = do
  g <- newStdGen
  let l = f g w h
  putStr $ showMaze l

-- Generate using recursive backtracking.
generateRBMaze :: Int -> Int -> IO ()
generateRBMaze = protoGenerateMaze recursiveBacktracker

-- Generate using Prim's algorithm.
generatePrimsMaze :: Int -> Int -> IO ()
generatePrimsMaze = protoGenerateMaze prims
