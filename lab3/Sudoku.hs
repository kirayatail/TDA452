module Sudoku where

import Test.QuickCheck
import Data.Char
import Data.List
import Data.Maybe
import Data.Ix
import Data.Function

-------------------------------------------------------------------------

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
 deriving ( Show, Eq )

example :: Sudoku
example = Sudoku {rows =
  [[Just 3,Just 6,Nothing,Nothing,Just 7,Just 1,Just 2,Nothing,Nothing],
   [Nothing,Just 5,Nothing,Nothing,Nothing,Nothing,Just 1,Just 8,Nothing],
   [Nothing,Nothing,Just 9,Just 2,Nothing,Just 4,Just 7,Nothing,Nothing],
   [Nothing,Nothing,Nothing,Nothing,Just 1,Just 3,Nothing,Just 2,Just 8],
   [Just 4,Nothing,Nothing,Just 5,Nothing,Just 2,Nothing,Nothing,Just 9],
   [Just 2,Just 7,Nothing,Just 4,Just 6,Nothing,Nothing,Nothing,Nothing],
   [Nothing,Nothing,Just 5,Just 3,Nothing,Just 8,Just 9,Nothing,Nothing],
   [Nothing,Just 8,Just 3,Nothing,Nothing,Nothing,Nothing,Just 6,Nothing],
   [Nothing,Nothing,Just 7,Just 6,Just 9,Nothing,Nothing,Just 4,Just 3]]}

-- allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku $ replicate 9 $ replicate 9 Nothing

-- isSudoku sud checks if su is really a valid representation of a sudoku
-- puzzle (size is 9x9 and content inside Maybe is in 1..9)
isSudoku :: Sudoku -> Bool
isSudoku (Sudoku su) = length su == 9 &&
                       and [length x == 9 &&
                            all validContent (catMaybes x) | x <- su]
  where validContent = inRange (1,9)


-- isSolved sud checks if sud is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved (Sudoku rows) = [] == filter (==Nothing) (concat rows)

-------------------------------------------------------------------------

-- printSudoku sud prints a representation of the sudoku sud on the screen
-- printSudoku :: Sudoku -> IO ()
printSudoku (Sudoku rows) = putStr $ unlines $ map stringRows rows
  where
        stringRows = map elemStr
        elemStr Nothing = '.'
        elemStr (Just i) = head $ show i

-- readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku fp =
  do
    s <- readFile fp
    return $ Sudoku $ map parseRow $ lines s
  where
    parseRow = map parseElement
    parseElement c | isDigit c = Just $ digitToInt c
                   | otherwise = Nothing

-------------------------------------------------------------------------

-- cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = frequency
  [(9, return Nothing),
  (1, do n <- choose (1,9)
         return $ Just n)]

-- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

-- Property of a sudoku
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku

-------------------------------------------------------------------------

type Block = [Maybe Int]

-- Block doesn't contain the same digit twice
-- Unique count plus 'Nothing' count should be 9 + 1
-- (one 'Nothing' in the unique list in any case).
isOkayBlock :: Block -> Bool
isOkayBlock b = length (nub (Nothing:b)) == 10 - length (filter (== Nothing) b)

-- Create a list of all blocks of a sudoku (rows, cols, 3x3-fields)
blocks :: Sudoku -> [Block]
blocks (Sudoku rows) = rows ++ transpose rows ++ boxes rows

-- Create blocks from the 3x3 squares by concatenating three elements in
-- three rows at a time for all nine rows
boxes :: [Block] -> [Block]
boxes [] = []
boxes rows = boxes' (take 3 rows) ++ boxes (drop 3 rows)
  where
    boxes' ([] : _) = []
    boxes' rows = concatMap (take 3) rows : boxes' (map (drop 3) rows)

-- A sudoku should contain exactly 3x9 blocks, each block contains 9 elements
prop_SudokuCorrectBlocks :: Sudoku -> Bool
prop_SudokuCorrectBlocks sudo = length (blocks sudo) == (3*9) &&
                                and [length b == 9 | b <- blocks sudo]

-- Check that a full sudoku is valid, all blocks should be OK
isOkay :: Sudoku -> Bool
isOkay sudo = and [isOkayBlock b | b <- blocks sudo]

-------------------------------------------------------------------------

-- Following the standard (row, pos)
type Pos = (Int, Int)

-- Return a list of positions that are still blank in a sudoku
blanks :: Sudoku -> [Pos]
blanks (Sudoku rows)= [(r, p) | r <- [0..8], p <- [0..8],
                       isNothing (rows !! r !! p) ]

prop_BlanksIsBlank :: Sudoku -> Bool
prop_BlanksIsBlank (Sudoku rows) = and [isNothing (rows !! r !! p) |
                                   (r, p) <- blanks (Sudoku rows)]
-- TODO: Möjligen ett redundant test? Proppen kollar inte ifall det finns blanka
-- som inte blanks hittar.

-- Set value from the tuple at position Int in the list, like a_arr[i] = a
(!!=) :: [a] -> (Int, a) -> [a]
[] !!= _          = []
(_:es) !!= (0, a) = a:es
(e:es) !!= (i, a) = e:(es !!= (i-1, a))
-- TODO: Also write (a) propert(y/ies) that state(s) the expected properties of
-- this function. Think about what can go wrong!

-- Set new cell value to a specified position in a Sudoku
update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update (Sudoku rows) (r,p) v = Sudoku (rows !!= (r,
                              (rows !! r) !!= (p, v)))

prop_UpdatedSudokuIsUpdated :: Sudoku -> Bool
prop_UpdatedSudokuIsUpdated = undefined

-- Returns a list of numbers that are valid candidates at the given position.
-- Subtract numbers already in the row, col and box from all possible numbers.
candidates :: Sudoku -> Pos -> [Int]
candidates s (r,p) = [1..9] \\ takenNums s (r,p)
  where
    takenNums s (r,p) = nub $ catMaybes $
                        (b !! r) ++ (b !! (9+p)) ++
                        (b !! (18 + (3 * quot r 3) + quot p 3))
    b = blocks s

-- Property that checks that all candidates are valid for all blanks in
-- a sudoku.
prop_validCandidates :: Sudoku -> Bool
prop_validCandidates s = all validCandidates $ blanks s
  where
    validCandidates p = and
      [isSudoku s' && isOkay s' |
        s' <- map (update s p . Just) (candidates s p)]

-------------------------------------------------------------------------

-- Picks out the blank with least candidates together with the candidates
bestPosition :: Sudoku -> (Pos, [Int])
bestPosition s = minimumBy (compare `on` (length . snd)) [(p, candidates s p) | p <- blanks s]

-- Same interface as above, picks the first (stupid) blank
firstPosition :: Sudoku -> (Pos, [Int])
firstPosition s = (pos, candidates s pos)
  where pos = head (blanks s)

-- The actual solving function
solveM :: Sudoku -> Maybe Sudoku
solveM sud | isSudoku sud && isOkay sud = solve' sud
           | otherwise                  = Nothing
  where
    solve' :: Sudoku -> Maybe Sudoku
    solve' s | isSolved s = Just s
             | otherwise  = solve'' s pos cands
      where (pos, cands) = bestPosition s
    solve'' :: Sudoku -> Pos -> [Int] -> Maybe Sudoku
    solve'' _ _ [] = Nothing
    solve'' s _ _       | isSolved s     = Just s
    solve'' s p (c:cs)  | isNothing newS = solve'' s p cs
                        | otherwise      = newS
      where newS = solve' $ update s p (Just c)

solve :: Sudoku -> Maybe Sudoku
solve = protoSolve bestPosition

slowSolve :: Sudoku -> Maybe Sudoku
slowSolve = protoSolve firstPosition

protoSolve :: (Sudoku -> (Pos, [Int])) -> Sudoku -> Maybe Sudoku
protoSolve pos sud | isSudoku sud && isOkay sud = solve' sud
                   | otherwise                  = Nothing
 where
  solve' s | isSolved s = Just s
  solve' s              = tryCandidates s $ pos s
  tryCandidates _ (_, []) = Nothing
  tryCandidates s (p, c:cs)
    | isJust s' && isSolved (fromJust s') = s'
    | otherwise = tryCandidates s (p, cs)
    where s' = solve' (update s p (Just c))

-- Solve Sudoku from file
readAndSolve :: FilePath -> IO ()
readAndSolve fp = do
  s <- readSudoku fp
  let res = solveM s
  case res of
    Just s' -> printSudoku s'
    Nothing -> print "(No solution)"


-- Check that first sudoku is a solution for the second.
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf s s' = isSudoku s
                    && isOkay s
                    && isSolved s
                    && nonBlanksMatch s s'
  where
    nonBlanksMatch (Sudoku rows) (Sudoku rows') =
      all nonBlanksMatch' $ zip rows rows'
    nonBlanksMatch' ([],[])                  = True
    nonBlanksMatch' (_:cs, Nothing:cs')      = nonBlanksMatch' (cs, cs')
    nonBlanksMatch' (c:cs, c':cs') | c == c' = nonBlanksMatch' (cs, cs')
    nonBlanksMatch' _                        = False

-- Property that that checks if a suggested solution (if a solution exists)
-- is actually a solution to the given sudoku.
-- prop_SolveSound :: Sudoku -> Property
prop_SolveSound s = solveSound $ solve s
  where
    solveSound (Just s') = isSolutionOf s' s
    solveSound _     = True
