-- Pure logic for Mazes.

module MazeLogic where

import Data.List
import System.Random
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

wallAt :: Maze -> Pos -> Direction -> Wall
wallAt m (x, y) d
  | d == U    = (v !! y) !! x
  | d == D    = (v !! (y + 1)) !! x
  | d == L    = (v !! x) !! y
  | otherwise = (v !! (x + 1)) !! y
  where
    h = horizontals m
    v = verticals m


canMove :: Maze -> Pos -> Direction -> Bool
canMove m p d = Open == wallAt m p d

positions :: Maze -> [Pos]
positions = undefined

possibleDirections :: Maze -> Pos -> [Direction]
possibleDirections m p = [d | d <- [U,D,L,R], canMove m p d]

isPerfect :: Maze -> Bool
isPerfect m = hasNoLoops [] (0,0)
  where
    hasNoLoops :: [Pos] -> Pos -> Bool
    hasNoLoops [] p =
      let
        newDirections = possibleDirections m p
        newPositions = map (neighborPos p) newDirections
      in
      hasNoLoops' [p] newPositions
    hasNoLoops visited p
      | p `elem` visited = False
      | otherwise        =
        let
          newDirections = possibleDirections m p
          lastPos = head visited
          filteredPositions = delete lastPos (map (neighborPos p) newDirections)
        in
        hasNoLoops' (p : visited) filteredPositions
    hasNoLoops' :: [Pos] -> [Pos] -> Bool
    hasNoLoops' visited [] = True
    hasNoLoops' visited toVisit = all (hasNoLoops visited) toVisit

recursiveBacktracker :: StdGen -> Int -> Int -> Maze
recursiveBacktracker g w h = rb g' unvisited visited (fullMaze w h)
  where
    (i, g') = randomR (0, (w*h)-1) g
    startPos  = unvisitedMaze w h !! i
    unvisited = unvisitedMaze w h \\ [startPos]
    visited   = [startPos]
    rb :: StdGen -> [Pos] -> [Pos] -> Maze -> Maze
    rb _ _  []     m = m
    rb g us (v:vs) m = case mDir of
                        (Just d)  -> rb g' (us \\ [neighborPos v d])
                                        (neighborPos v d:v:vs)
                                        (removeWall m v d)
                        Nothing -> rb g' us vs m
      where
        (mDir, g') = pickDirection g us v

-- Return a 'random' direction that has neighboring position in the list
pickDirection :: StdGen -> [Pos] -> Pos -> (Maybe Direction, StdGen)
pickDirection g u p = pickElement g [d | d <- [U,D,L,R], neighborPos p d `elem` u]


pickElement :: StdGen -> [a] -> (Maybe a, StdGen)
pickElement g [] = (Nothing, g)
pickElement g es = (Just (es !! i), g')
  where
    (i, g') = randomR (0, length es - 1) g


neighborPos :: Pos -> Direction -> Pos
neighborPos (x,y) U = (x, y-1)
neighborPos (x,y) D = (x, y+1)
neighborPos (x,y) L = (x-1, y)
neighborPos (x,y) R = (x+1, y)

unvisitedMaze :: Int -> Int -> [Pos]
unvisitedMaze w h = [(x,y) | x <-[0..w-1], y <- [0..h-1]]
