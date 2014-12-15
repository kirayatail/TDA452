import Haste
import Haste.Graphics.Canvas

height = 500
width = 500

-- Create a canvas to draw on.
newCanvas :: Double -> Double -> IO Elem
newCanvas w h = do
  canvas <- newElem "canvas"
  setStyle canvas "border" "1px solid black"
  setStyle canvas "display" "block"
  setStyle canvas "margin" "auto"
  setStyle canvas "background-color" "pink"
  setProp canvas "width" (show w)
  setProp canvas "height" (show h)
  return canvas

main = do
  canvasElem <- newCanvas width height
  Just canvas <- getCanvas canvasElem
  setProp canvasElem "height" (show 250)
  setChildren documentBody [canvasElem]
  return ()
