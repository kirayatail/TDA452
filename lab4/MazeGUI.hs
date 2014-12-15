import Haste
import Haste.Graphics.Canvas
import MazeLogic
import Data.List

cellSize :: Int
cellSize = 30

cs :: Double
cs = fromIntegral cellSize

-- Create a canvas to draw on.
newCanvas :: Double -> Double -> IO Elem
newCanvas w h = do
  canvas <- newElem "canvas"
  setStyle canvas "border" "1px solid black"
  setStyle canvas "display" "block"
  setStyle canvas "margin" "auto"
  setProp canvas "width" (show w)
  setProp canvas "height" (show h)
  return canvas

-- Create textfield, used for dimension input
newTextField :: String -> IO Elem
newTextField ident = do
  input <- newElem "input"
  setProp input "type" "text"
  setProp input "id" ident
  setProp input "name" ident
  setProp input "placeholder" ident
  setStyle input "width" "6em"
  setStyle input "margin-right" "1em"
  return input

-- Create a simple button with text 'Generate Maze'
newSubmitButton :: IO Elem
newSubmitButton = do
  btn <- newElem "button"
  setProp btn "id" "submitButton"
  setProp btn "name" "submitButton"
  text <- newTextElem "Generate Maze"
  setChildren btn [text]
  return btn

-- Create selection box for choosing algorithm
newAlgoSelector :: IO Elem
newAlgoSelector = do
  sel <- newElem "select"
  rb <- newElem "option"
  prim <- newElem "option"
  setProp rb "value" "rb"
  setProp prim "value" "prim"
  text <- newTextElem "Recursive Backtracker"
  setChildren rb [text]
  text <- newTextElem "Prim's algorithm"
  setChildren prim [text]
  setChildren sel [rb, prim]
  return sel

-- Generate a container for user controls
settingsDiv :: IO Elem
settingsDiv = do
  d <- newElem "div"
  setStyle d "margin" "auto"
  setStyle d "width" $ show 500 ++"px"
  setStyle d "margin-bottom" "1em"
  return d

-- Instantiate all elements and bind generateMaze to button
main = do
  cElem <- newCanvas 500.0 500.0
  sDivElem <- settingsDiv
  wElem <- newTextField "width"
  hElem <- newTextField "height"
  btn <- newSubmitButton
  sel <- newAlgoSelector
  setChildren sDivElem [wElem, hElem, sel, btn]
  Just canvas <- getCanvas cElem
  setChildren documentBody [sDivElem, cElem]
  onEvent btn OnClick $ \_ -> generateMaze canvas cElem wElem hElem sel
  return ()

-- drawing directives for a single wall, True means the wall is vertical
wall :: Bool -> Pos -> Picture ()
wall vert (x',y') = color (RGB 0 0 0) $
  stroke $ if vert then
            line (x*cs, y*cs) (x*cs, (y+1)*cs)
           else
            line (x*cs, y*cs) ((x+1)*cs, y*cs)
    where
      x = fromIntegral x'
      y = fromIntegral y'

-- Turn a grid of walls into a list of positions where a wall should be drawn
wallPositions :: [[Wall]] -> [Pos]
wallPositions rows = [(x,y) | x <- [0.. length rows - 1],
                              y <- [0.. length (rows !! x) -1],
                              rows !! x !! y == Blocked]

-- Extract horizontal and vertical walls from a maze and render on canvas
renderLabyrinth :: Canvas -> Maze -> IO ()
renderLabyrinth can m =
  let hs = wallPositions $ transpose $ horizontals m in
  let vs = wallPositions $ verticals m in
  render can $ do
    sequence_ $ fmap (wall True) vs
    sequence_ $ fmap (wall False) hs
    wall True (0,0)

-- Get parameters from HTML form, generate a maze and render it to the canvas
generateMaze :: Canvas -> Elem -> Elem -> Elem -> Elem -> (Int, Int) ->IO ()
generateMaze canvas cElem xElem yElem fElem _ = do
  mx <- getValue xElem
  my <- getValue yElem
  mf <- getValue fElem
  g <- newSeed
  let (x,y) = case (mx, my) of
            (Just x, Just y) -> (x,y)
            _                -> (6,6)
  let f = case mf of
          Just "prim" -> prims g
          Just "rb"   -> recursiveBacktracker g
          _           -> emptyMaze
  let m = f x y
  setProp cElem "height" $ show (y * cellSize)
  setProp cElem "width" $ show (x * cellSize)
  renderLabyrinth canvas m
  return ()
