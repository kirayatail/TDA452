-- Pure logic for Mazes.

module MazeLogic where

data Maze = Maze Int

data Direction = Up | Down | Left | Right

type Pos = (Int,Int)

emptyMaze, fullMaze :: Int -> Int -> Maze
emptyMaze = undefined
fullMaze = undefined

addWall, removeWall :: Maze -> Pos -> Direction
addWall = undefined
removeWall = undefined

canMove :: Maze -> Pos -> Direction -> Bool
canMove = undefined

positions :: Maze -> [Pos]
positions = undefined

isPerfect :: Maze -> bool
isPerfect = undefined
