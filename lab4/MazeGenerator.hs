
module MazeGenerator where
import MazeLogic
import System.Random

protoGenerateMaze :: (StdGen -> Int -> Int -> Maze) -> Int -> Int -> IO ()
protoGenerateMaze f w h = do
  g <- newStdGen
  let l = f g w h
  putStr $ showMaze l

generateRBMaze :: Int -> Int -> IO ()
generateRBMaze = protoGenerateMaze recursiveBacktracker

generatePrimsMaze :: Int -> Int -> IO ()
generatePrimsMaze = protoGenerateMaze prims
