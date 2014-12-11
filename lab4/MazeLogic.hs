-- Pure logic for Mazes.

module MazeLogic where

import Data.List

data Maze = Maze { vertical :: [[Wall]], horizontal :: [[Wall]]}
  deriving ( Show, Eq )

data Wall = Open | Blocked
  deriving ( Show, Eq )

data Direction = U | D | L | R
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

recursiveBacktracker :: Int -> Int -> Maze
recursiveBacktracker w h = rb unvisited visited (fullMaze w h)
  where
    startPos  = (0,0)
    unvisited = unvisitedMaze w h \\ [startPos]
    visited   = [startPos]
    rb :: [Pos] -> [Pos] -> Maze -> Maze
    rb _  []     m = m
    rb us (v:vs) m = case mDir of
                        (Just d)  -> rb (us \\ [neighborPos v d])
                                        (neighborPos v d:v:vs)
                                        m
                        Nothing -> rb us vs m
      where
        mDir = pickDirection us v

-- Return a 'random' direction that has neighboring position in the list
pickDirection :: [Pos] -> Pos -> Maybe Direction
pickDirection u p = case [d | d <- [U,D,L,R], neighborPos p d `elem` u] of
                         []    -> Nothing
                         (d:_) -> Just d

neighborPos :: Pos -> Direction -> Pos
neighborPos (x,y) U = (x, y-1)
neighborPos (x,y) D = (x, y+1)
neighborPos (x,y) L = (x-1, y)
neighborPos (x,y) R = (x+1, y)

unvisitedMaze :: Int -> Int -> [Pos]
unvisitedMaze w h = [(x,y) | x <-[0..w-1], y <- [0..h-1]]
