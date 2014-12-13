
module MazeGenerator where
import MazeLogic
import System.Random

generateRBMaze :: Int -> Int -> IO ()
generateRBMaze w h = do
  g <- newStdGen
  let l = recursiveBacktracker g w h
  putStr $ showMaze l
