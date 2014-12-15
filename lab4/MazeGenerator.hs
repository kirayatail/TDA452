
module MazeGenerator where
import MazeLogic
import Haste
import Test.QuickCheck

-- Prototype for generating a maze with pluggable algorithm.
protoGenerateMaze :: (Seed -> Int -> Int -> Maze) -> Int -> Int -> IO ()
protoGenerateMaze f w h = do
  g <- newSeed
  let l = f g w h
  putStr $ showMaze l

-- Generate using recursive backtracking.
generateRBMaze :: Int -> Int -> IO ()
generateRBMaze = protoGenerateMaze recursiveBacktracker

-- Generate using Prim's algorithm.
generatePrimsMaze :: Int -> Int -> IO ()
generatePrimsMaze = protoGenerateMaze prims

-- Borrowed from the BlackJack lab.
instance Arbitrary Seed where
  arbitrary = do
    n <- arbitrary
    return (mkSeed n)

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
