-- Pure logic for Mazes.

module MazeLogic where

import Data.List

data Maze = Maze { vertical :: [[Wall]], horizontal :: [[Wall]]}
  deriving ( Show, Eq )

data Wall = Open | Blocked
  deriving ( Show, Eq )

data Direction = Up | Down | Left | Right
  deriving ( Show, Eq )

type Pos = (Int,Int)

verticals, horizontals :: Maze -> [[Wall]]
verticals (Maze v _) = v
horizontals (Maze _ h) = h

showMaze :: Maze -> String
showMaze m = showHorizontal vs hs
  where
    vs = transpose $ verticals m
    hs = horizontals m
    showHorizontal [] []        = []
    showHorizontal vs' (h:hs') = concatMap showHorizontalWall h ++ "\n" ++showVertical vs' hs'
    showVertical [] []        = []
    showVertical (v:vs') hs' = concatMap showVerticalWall v ++ "\n" ++ showHorizontal vs' hs'
    showVerticalWall Open = "   "
    showVerticalWall Blocked = "|  "
    showHorizonalWall Open = "   "
    showHorizontalWall Blocked = " ——"


emptyMaze, fullMaze :: Int -> Int -> Maze
emptyMaze = undefined
-- fullMaze x y = Maze {vertical = (replicate x + 1 $ wallList x) horizontal = (replicate y + 1 $ wallList y)}
fullMaze x y = Maze {vertical = replicate (x + 1) $ wallList y, horizontal = replicate (y + 1) $ wallList x}
  where
    wallList n = replicate n Blocked

addWall, removeWall :: Maze -> Pos -> Direction
addWall = undefined
removeWall = undefined

canMove :: Maze -> Pos -> Direction -> Bool
canMove = undefined

positions :: Maze -> [Pos]
positions = undefined

isPerfect :: Maze -> bool
isPerfect = undefined
