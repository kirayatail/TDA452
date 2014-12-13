
module MazeGenerator where
import MazeLogic
import System.Random

generateRBMaze :: Int -> Int -> IO ()
generateRBMaze w h = do
  g <- newStdGen
  putStr $ showMaze $ recursiveBacktracker g w h
