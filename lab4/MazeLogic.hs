-- Pure logic for Mazes.

module MazeLogic where

import Data.List
import Test.QuickCheck

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
    showHorizontal vs' (h:hs') =
      concatMap showHorizontalWall h
        ++ "\n" ++ showVertical vs' hs'
    showVertical [] []        = []
    showVertical (v:vs') hs' =
      concatMap showVerticalWall v
        ++ "\n" ++ showHorizontal vs' hs'
    showVerticalWall Open = "   "
    showVerticalWall Blocked = "|  "
    showHorizontalWall Open = "   "
    showHorizontalWall Blocked = " ——"

emptyMaze, fullMaze :: Int -> Int -> Maze
emptyMaze x y = Maze {vertical = wallList x y, horizontal = wallList y x}
  where
    wallList n m =
      replicate m Blocked
      : replicate (n - 1) (replicate m Open)
      ++ [replicate m Blocked]
fullMaze x y = Maze {vertical = wallList x y, horizontal = wallList y x}
  where
    wallList n m = replicate (n + 1) $ replicate m Blocked

-- Set value from the tuple at position Int in the list, like a_arr[i] = a
(!!=) :: [a] -> (Int, a) -> [a]
[] !!= _          = []
(_:es) !!= (0, a) = a:es
(e:es) !!= (i, a) = e:(es !!= (i-1, a))

-- Check that an updated list has the same lenth, that the value has been
-- updated and that the rest of the list is unmodified.
prop_updateList :: NonEmptyList Int -> Int -> Property
prop_updateList (NonEmpty xs) x =
  forAll (choose (0, length xs - 1)) $ \i ->
  let xs' = xs !!= (i,x) in
  length xs == length xs'
  && x == xs' !! i
  && take i xs == take i xs'
  && drop (i + 1) xs == drop (i + 1) xs'

updateWall :: Maze -> Pos -> Direction -> Wall -> Maze
updateWall m (x, y) d w
  | d == U    = Maze {vertical = v, horizontal = update h (y, x) w}
  | d == D    = Maze {vertical = v, horizontal = update h (y + 1, x) w}
  | d == L    = Maze {vertical = update v (x, y) w, horizontal = h}
  | otherwise = Maze {vertical = update v (x + 1, y) w, horizontal = h}
  where
    h = horizontals m
    v = verticals m
    update xs (n,m) d = xs !!= (n, (xs !! n) !!= (m, d))


addWall, removeWall :: Maze -> Pos -> Direction -> Maze
addWall m p d = updateWall m p d Blocked
removeWall m p d = updateWall m p d Open

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
                                        (removeWall m v d)
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
