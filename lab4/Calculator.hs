import Control.Monad (when)

import Haste hiding (eval)
import Haste.DOM
import Haste.Events
import Haste.Graphics.Canvas
import Data.Ix
import Pages
import Data.Maybe

import Expr



canWidth  = 300
canHeight = 300

readAndDraw :: Elem -> Canvas -> IO ()
readAndDraw e c = do s      <- getProp e "value"
                     height <- getProp c "height"
                     width  <- getProp c "width"
                     let exp = fromJust (readExpr s)
                     let ps = points exp ((read width)/canWidth) (read width, read height)
                     render c (stroke $ path ps)

readZoomAndDraw  :: Elem -> Canvas -> IO ()
readZoomAndDraw e c = do s      <- getProp e "value"
                         height <- getProp c "height"
                         width  <- getProp c "width"
                         scale  <- getProp e "zoom"
                         let sca = eval (fromJust (readExpr s)) 0
                         let exp = fromJust (readExpr s)
                         let ps = points exp (sca*(read width)/canWidth) (read width, read height)
                         render c (stroke $ path ps)

points :: Expr -> Double -> (Int,Int) -> [Point]
points e scale (w,h) = [formXY (i,eval e i) scale (w',h') | i <- [0..w']]
    where w'  = realToFrac w
          h'  = realToFrac h
          up  = (w'*scale)/2
          low = -up

formXY :: (Double, Double) -> Double -> (Double,Double) -> Point
formXY (x,y) scale (width, height) = (xNew,yNew)
    where xNew = x/scale + width/2
          yNew = height/2 - y/scale

main = do
    -- Elements
    canvas  <- mkCanvas (round canWidth) canHeight   -- The drawing area
    fx      <- mkHTML "<i>f</i>(<i>x</i>)="  -- The text "f(x)="
    input   <- mkInput 20 "x"                -- The formula input
    draw    <- mkButton "Draw graph"         -- The draw button
    diff    <- mkButton "Draw differentiated graph"         -- The draw button
    zoom    <- mkInput 20 "Zoom"
      -- The markup "<i>...</i>" means that the text inside should be rendered
      -- in italics.

    -- Layout
    formula <- mkDiv
    row formula [fx,input]
    column documentBody [canvas,formula,draw,zoom]

    -- Styling
    setStyle documentBody "backgroundColor" "lightblue"
    setStyle documentBody "textAlign" "center"
    setStyle input "fontSize" "14pt"
    focus input
    select input

    -- Interaction
    Just can <- getCanvas canvas
    onEvent draw  Click $ \_    -> readAndDraw input can
    onEvent input KeyUp $ \code -> when (code==13) $ readAndDraw input can
    onEvent diff  Click $ \_    -> readAndDrawDiff input can
    onEvent zoom  KeyUp $ \code -> when (code==13) $ readZoomAndDraw zoom can
      -- "Enter" key has code 13

readAndDrawDiff :: Elem -> Canvas -> IO ()
readAndDrawDiff e c = do 
                     s     <- getProp e "value"
                     height<- getProp c "height"
                     width <- getProp c "width"
                     let exp = differentiate $ fromJust (readExpr s)
                     _     <- setProp e "value" (showExpr exp)
                     let ps = points exp ((read width)/canWidth) (read width, read height)
                     render c (stroke $ path ps)

