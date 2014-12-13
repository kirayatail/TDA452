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
data Direction = N | S | W | E
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
  | d == N    = Maze {vertical = v, horizontal = update h (y, x) w}
  | d == S    = Maze {vertical = v, horizontal = update h (y + 1, x) w}
  | d == W    = Maze {vertical = update v (x, y) w, horizontal = h}
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
  | d == N = (h !! y) !! x
  | d == S = (h !! (y + 1)) !! x
  | d == W = (v !! x) !! y
  | otherwise = (v !! (x + 1)) !! y
  where
    h = horizontals m
    v = verticals m

-- Determine if a move is allowed.
canMove :: Maze -> Pos -> Direction -> Bool
canMove m p d = Open == wallAt m p d

-- Determine which directions a move is allowed in.
possibleDirections :: Maze -> Pos -> [Direction]
possibleDirections m p = [d | d <- [N,S,W,E], canMove m p d]

-- Determine which positions you can move to.
possiblePositions :: Maze -> Pos -> [Pos]
possiblePositions m p = map (neighborPos p) $ possibleDirections m p

-- Return all directions in which a given position's neighbors exist
-- in the provided list.
directionsInList :: [Pos] -> Pos -> [Direction]
directionsInList u p = [d | d <- [N,S,W,E], neighborPos p d `elem` u]

-- For a given position, return all its neighbors that exist in a provided list
neighborsInList :: [Pos] -> Pos -> [Pos]
neighborsInList u p = map (neighborPos p) $ directionsInList u p

-- Returns an arbitrary element from a list, or Nothing if the list is empty.
pickElement :: StdGen -> [a] -> (Maybe a, StdGen)
pickElement g [] = (Nothing, g)
pickElement g es = (Just (es !! i), g')
  where
    (i, g') = randomR (0, length es - 1) g

-- Returns the position of a 'neighbor' given a position and a direction
neighborPos :: Pos -> Direction -> Pos
neighborPos (x,y) N = (x, y-1)
neighborPos (x,y) S = (x, y+1)
neighborPos (x,y) W = (x-1, y)
neighborPos (x,y) E = (x+1, y)

positions :: Maze -> [Pos]
positions (Maze vs hs) = [(x,y) | x <- [0..(length vs -2)], y <- [0..(length hs -2)]]

-- Check that the labyrinth is rectangular and is surrounded by walls.
isOkay :: Maze -> Bool
isOkay m = w > 0
        && h > 0
        && length ho == (h + 1)
        && length ve == (w + 1)
        && all ((== h) . length) ve
        && all ((== w) . length) ho
        && onlyBlocked (head ve)
        && onlyBlocked (head ho)
        && onlyBlocked (last ve)
        && onlyBlocked (last ho)
 where
  w = width m
  h = height m
  ho = horizontals m
  ve = verticals m
  onlyBlocked = all (== Blocked)

-- Checks that a maze is perfect by visiting all nodes and verifying that
-- there are no loops or unreachable positions.
-- Uses a modified Recursive Backtracker to find and visit all positions
isPerfect :: Maze -> Bool
isPerfect m = hasNoLoops [] (0,0) && allPosReachable
  where
    hasNoLoops :: [Pos] -> Pos -> Bool
    hasNoLoops visited p
      | p `elem` visited = False
      | otherwise        =
          let toVisit = whereToNext p visited in
        hasNoLoops' (p : visited) toVisit
        where
          hasNoLoops' :: [Pos] -> [Pos] -> Bool
          hasNoLoops' visited [] = True
          hasNoLoops' visited toVisit = all (hasNoLoops visited) toVisit
          whereToNext :: Pos -> [Pos] -> [Pos]
          whereToNext p [] = possiblePositions m p
          whereToNext p visited = delete (head visited) $ possiblePositions m p
    allPosReachable :: Bool
    allPosReachable = [] == rbs (delete (0,0) $ positions m) [(0,0)]
      where
        rbs :: [Pos] -> [Pos] -> [Pos]
        rbs u [] = u
        rbs u (v:vs) =
          let ds = [d | d <- directionsInList u v, canMove m v d] in
                        if null ds then
                          rbs u vs
                        else
                          rbs (u \\ [neighborPos v $ head ds])
                              (neighborPos v (head ds):v:vs)

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

-- Tests if the recursive backtracker algorithm produces mazes that are valid.
prop_RBIsOkay :: StdGen ->Int -> Int-> Bool
prop_RBIsOkay g x y = isOkay
  $ recursiveBacktracker g (1 + (x `mod` 50)) (1 + (y `mod` 50))

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

-- Tests if Prim's algorithm produces mazes that are valid.
prop_PrimsIsOkay :: StdGen -> Int -> Int-> Bool
prop_PrimsIsOkay g x y = isOkay
  $ prims g (1 + (x `mod` 50)) (1 + (y `mod` 50))

-- Tests if Prim's algorithm produces perfect mazes.
prop_PrimsIsPerfect :: StdGen -> Int -> Int-> Bool
prop_PrimsIsPerfect g x y = isPerfect
  $ prims g (1 + (x `mod` 50)) (1 + (y `mod` 50))
