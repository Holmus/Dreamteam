import Control.Monad (when)

import Haste hiding (eval)
import Haste.DOM
import Haste.Events
import Haste.Graphics.Canvas
import Data.Ix
import Data.Char
import Pages
import Data.Maybe

import Expr

canWidth  = 300
canHeight = 300

readZoomAndDraw :: Elem -> Elem -> Canvas -> IO ()
readZoomAndDraw z v c = do s    <- getProp v "value"
                           height <- getProp c "height"
                           width  <- getProp c "width"
                           zoom   <- getProp z "value"
                           let scale = eval (fromJust (readExpr zoom)) 0
                           let exp = fromJust (readExpr s)
                           let ps = points exp (1/scale) (read width, read height)
                           render c (stroke $ path ps)
 
points :: Expr -> Double -> (Int,Int) -> [Point]
points e scale (w,h) = [formXY (i,eval e i) scale (w',h') | i <- [(-w'),(-w'+scale)..w']]
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
    diff    <- mkButton "Draw differentiated graph"         -- The draw button
    scaleT  <- mkHTML "scale="  -- The text "scale="
    zoom    <- mkInput 20 "1"
    scale   <- mkButton "Draw graph"         -- The draw button
      -- The markup "<i>...</i>" means that the text inside should be rendered
      -- in italics.

    -- Layout
    formula <- mkDiv
    row formula [fx,input]
    szoom <- mkDiv
    row szoom [scaleT, zoom]
    buttons <- mkDiv
    row buttons [scale, diff]
    column documentBody [canvas,formula,szoom, buttons]

    -- Styling
    setStyle documentBody "backgroundColor" "lightblue"
    setStyle documentBody "textAlign" "center"
    setStyle input "fontSize" "14pt"
    focus input
    select input

    -- Interaction
    Just can <- getCanvas canvas
    onEvent diff  Click $ \_    -> readAndDrawDiff zoom input can
    onEvent scale Click $ \_    -> readZoomAndDraw zoom input can
    onEvent zoom  KeyUp $ \code -> when (code==13) $ readZoomAndDraw zoom input can
      -- "Enter" key has code 13

readAndDrawDiff :: Elem -> Elem -> Canvas -> IO ()
readAndDrawDiff z v c = do s    <- getProp v "value"
                           height <- getProp c "height"
                           width  <- getProp c "width"
                           zoom   <- getProp z "value"
                           let exp = differentiate $ fromJust (readExpr s)
                           _ <- setProp v "value" (showExpr exp)
                           let scale = eval (fromJust (readExpr zoom)) 0
                           let ps = points exp (1/scale) (read width, read height)
                           render c (stroke $ path ps)

