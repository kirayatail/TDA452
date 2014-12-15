import Haste
import Haste.Graphics.Canvas
import MazeLogic
import Data.List

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

-- Create textfield
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

newSubmitButton :: IO Elem
newSubmitButton = do
  btn <- newElem "button"
  setProp btn "id" "submitButton"
  setProp btn "name" "submitButton"
  text <- newTextElem "Generate Maze"
  setChildren btn [text]
  return btn

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

settingsDiv :: IO Elem
settingsDiv = do
  d <- newElem "div"
  setStyle d "margin" "auto"
  setStyle d "width" $ show 500 ++"px"
  setStyle d "margin-bottom" "1em"
  return d

main = do
  canvasElem <- newCanvas 500.0 500.0
  sDivElem <- settingsDiv
  xInputElem <- newTextField "width"
  yInputElem <- newTextField "height"
  btn <- newSubmitButton
  sel <- newAlgoSelector
  setChildren sDivElem [xInputElem, yInputElem, sel, btn]
  Just canvas <- getCanvas canvasElem
  setChildren documentBody [sDivElem, canvasElem]
  onEvent btn OnClick $ \_ -> generateLabyrinth canvas canvasElem xInputElem yInputElem sel
  return ()

wall :: Bool -> Pos -> Picture ()
wall vert (x',y') = color (RGB 0 0 0) $
                    stroke $ if vert then
                              line (x*20, y*20) (x*20, (y+1)*20)
                            else
                              line (x*20, y*20) ((x+1)*20, y*20)
    where
      x = fromIntegral x'
      y = fromIntegral y'

wallPositions :: [[Wall]] -> [Pos]
wallPositions rows = [(x,y) | x <- [0.. length rows - 1], y <- [0.. length (rows !! x) -1], rows !! x !! y == Blocked]


-- | Render the game's state to a canvas.
renderLabyrinth :: Canvas -> Maze -> IO ()
renderLabyrinth can m =
  let hs = wallPositions $ transpose $ horizontals m in
  let vs = wallPositions $ verticals m in
  render can $ do
    sequence_ $ fmap (wall True) vs
    sequence_ $ fmap (wall False) hs
    wall True (0,0)


generateLabyrinth :: Canvas -> Elem -> Elem -> Elem -> Elem -> (Int, Int) ->IO ()
generateLabyrinth canvas cElem xElem yElem fElem _ = do
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
  setProp cElem "height" $ show (20 * y)
  setProp cElem "width" $ show (20 * x)
  renderLabyrinth canvas m
  return ()
