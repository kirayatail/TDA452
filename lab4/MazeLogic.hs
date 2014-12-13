-- Pure logic for Mazes.

module MazeLogic where

import Data.List
import Data.Maybe
import System.Random
import Test.QuickCheck

-- A maze is represented with two two dimensional lists representing the walls
-- that run vertically and horizontally.
data Maze = Maze { vertical :: [[Wall]], horizontal :: [[Wall]]}
  deriving ( Show, Eq )

-- The space for a wall can either be open or blocked.
data Wall = Open | Blocked
  deriving ( Show, Eq )

-- The directions in a labyrinth are up, down left and right.
-- We chose  to go with only one letter since Left and Right are reserved.
data Direction = U | D | L | R
  deriving ( Show, Eq )

-- A position in a labyrinth can be represented as a tuple.
type Pos = (Int,Int)

-- Get all walls of a certain orientation.
verticals, horizontals :: Maze -> [[Wall]]
verticals (Maze v _) = v
horizontals (Maze _ h) = h

-- Format a maze as an ASCII string.
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

-- Generates a maze with no walls respectively only walls.
-- Note that even an empty maze has walls around it.
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

-- Get the width and height of a maze.
height, width  :: Maze -> Int
height (Maze _ h) = length h - 1
width (Maze v _)  = length v - 1


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

-- Update the type of a wall.
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

-- Shortcut to  updateWall for adding and removing walls.
addWall, removeWall :: Maze -> Pos -> Direction -> Maze
addWall m p d = updateWall m p d Blocked
removeWall m p d = updateWall m p d Open

-- See what type a wall in a direction has.
wallAt :: Maze -> Pos -> Direction -> Wall
wallAt m (x, y) d
  | d == U = (v !! y) !! x
  | d == D = (v !! (y + 1)) !! x
  | d == L = (h !! x) !! y
  | otherwise = (h !! (x + 1)) !! y
  where
    h = horizontals m
    v = verticals m

-- Determine if a move is allowed.
canMove :: Maze -> Pos -> Direction -> Bool
canMove m p d = Open == wallAt m p d

-- Determine which directions a move is allowed in.
possibleDirections :: Maze -> Pos -> [Direction]
possibleDirections m p = [d | d <- [U,D,L,R], canMove m p d]

-- Determine which positions you can move to.
possiblePositions :: Maze -> Pos -> [Pos]
possiblePositions m p = map (neighborPos p) $ possibleDirections m p

directionsInList :: [Pos] -> Pos -> [Direction]
directionsInList u p = [d | d <- [U,D,L,R], neighborPos p d `elem` u]

neighborsInList :: [Pos] -> Pos -> [Pos]
neighborsInList u p = map (neighborPos p) $ directionsInList u p

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

positions :: Maze -> [Pos]
positions (Maze vs hs) = [(x,y) | x <- [0..(length vs -2)], y <- [0..(length hs -2)]]

-- Checks that a maze is perfect by visiting all nodes and verifying that
-- there are no loops or unreachable positions.
isPerfect :: Maze -> Bool
isPerfect m = hasNoLoops [] (0,0)
  where
    hasNoLoops :: [Pos] -> Pos -> Bool
    hasNoLoops visited p
      | p `elem` visited = False
      | otherwise        =
        let toVisit = whereToNext p visited in
        hasNoLoops' (p : visited) toVisit
    hasNoLoops' :: [Pos] -> [Pos] -> Bool
    hasNoLoops' visited [] = True
    hasNoLoops' visited toVisit = all (hasNoLoops visited) toVisit
    whereToNext :: Pos -> [Pos] -> [Pos]
    whereToNext p [] = possiblePositions m p
    whereToNext p visited = delete (head visited) $ possiblePositions m p


recursiveBacktracker :: StdGen -> Int -> Int -> Maze
recursiveBacktracker g w h = rb g' unvisited visited (fullMaze w h)
  where
    (mStartPos, g') = pickElement g $ positions (fullMaze w h)
    startPos = fromJust mStartPos
    unvisited      = positions (fullMaze w h) \\ [startPos]
    visited   = [startPos]
    rb :: StdGen -> [Pos] -> [Pos] -> Maze -> Maze
    rb _ _  []     m = m
    rb g us (v:vs) m = case mDir of
                        (Just d)  -> rb g' (us \\ [neighborPos v d])
                                        (neighborPos v d:v:vs)
                                        (removeWall m v d)
                        Nothing -> rb g' us vs m
      where
        (mDir, g') = pickElement g $ directionsInList us v

-- Borrowed from the BlackJack lab.
instance Arbitrary StdGen where
  arbitrary = do
    n <- arbitrary
    return (mkStdGen n)

-- Tests if the recursive backtracker algorithm produces perfect mazes.
prop_RBIsPerfect :: StdGen ->Int -> Int-> Bool
prop_RBIsPerfect g x y = isPerfect
  $ recursiveBacktracker g (1 + (x `mod` 50)) (1 + (y `mod` 50))

prims :: StdGen -> Int -> Int -> Maze
prims g w h = prim g' unvisited visited frontier (fullMaze w h)
  where
    (mStartPos, g') = pickElement g $ positions $ fullMaze w h
    startPos = fromJust mStartPos
    unvisited = positions (fullMaze w h) \\ (startPos:frontier)
    visited   = [startPos]
    frontier  = neighborsInList (positions (fullMaze w h)) startPos
    prim :: StdGen -> [Pos] -> [Pos] -> [Pos] -> Maze -> Maze
    prim _ _  _  [] m = m
    prim g us vs fs m = let nFront = neighborsInList us p in
                        prim g'
                        (us \\ nFront)
                        (p:vs)
                        (delete p $ nFront `union` fs)
                        (removeWall m p d)
      where
        p = fromJust mPos
        d = fromJust mDir
        (mPos, g') = pickElement g fs
        (mDir, g'') = pickElement g' $ directionsInList vs p

-- Tests if Prim's algorithm produces perfect mazes.
prop_PrimsIsPerfect :: StdGen -> Int -> Int-> Bool
prop_PrimsIsPerfect g x y = isPerfect
  $ prims g (1 + (x `mod` 50)) (1 + (y `mod` 50))
