import Haste
import Haste.Graphics.Canvas
import MazeLogic

height = 500
width = 500

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
  return input

submitButton :: IO Elem
submitButton = do
  btn <- newElem "button"
  setProp btn "id" "submitButton"
  setProp btn "name" "submitButton"
  text <- newTextElem "Generate Maze"
  setChildren btn [text]
  return btn

settingsDiv :: IO Elem
settingsDiv = do
  d <- newElem "div"
  setStyle d "margin" "auto"
  setStyle d "width" $ show 500 ++"px"
  setStyle d "margin-bottom" "1em"
  return d

main = do
  canvasElem <- newCanvas width height
  sDivElem <- settingsDiv
  xInputElem <- newTextField "width"
  yInputElem <- newTextField "height"
  btn <- submitButton
  setChildren sDivElem [xInputElem, yInputElem, btn]
  Just canvas <- getCanvas canvasElem
  setProp canvasElem "height" (show 250)
  setChildren documentBody [sDivElem, canvasElem]
  onEvent btn OnClick $ generateLabyrinth canvas xInputElem yInputElem
  return ()

generateLabyrinth canvas xElem yElem = do
  mx <- getValue xElem
  my <- getValue yElem
  g <- newStdGen
  case (mx, my) of
    (Just x, Just y) -> return emptyMaze x y
    _                -> return ()
  return ()
