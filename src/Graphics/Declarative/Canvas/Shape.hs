module Graphics.Declarative.Canvas.Shape where

import qualified Graphics.Rendering.Canvas as Canvas

import Graphics.Declarative.Border as Border
import Graphics.Declarative.Bordered

import Linear

newtype Shape = Shape { renderShape :: Canvas.Canvas () }

circle :: Double -> Bordered Shape
circle radius = Bordered (Border.circle radius) shape
  where shape = Shape $ Canvas.beginPath >> Canvas.arc 0 0 radius 0 (2*pi)

rectangle :: Double -> Double -> Bordered Shape
rectangle width height = rectangleFromBB (V2 (-width/2) (-height/2), V2 (width/2) (height/2))

rectangleFromBB :: (V2 Double, V2 Double) -> Bordered Shape
rectangleFromBB corners@(V2 l t, V2 r b) = Bordered (Border.fromBoundingBox corners) shape
  where
    shape = Shape $ Canvas.beginPath >> Canvas.rect l t (r-l) (b-t)

roundedRectangle :: Double -> Double -> Double -> Bordered Shape
roundedRectangle radius width height = roundedRectangleFromBB radius (V2 0 0, V2 width height)

roundedRectangleFromBB :: Double -> (V2 Double, V2 Double) -> Bordered Shape
roundedRectangleFromBB radius boundingBox@(V2 left up, V2 right down)
  | radius > width/2 || radius > height/2 = roundedRectangleFromBB (min (width/2) (height/2)) boundingBox
  | otherwise = Bordered hull $ Shape render
  where
    width = right-left
    height = down-up

    innerLeft = left+radius
    innerUp = up+radius
    innerRight = right-radius
    innerDown = down-radius

    hull = Border.padded radius $ Border.fromBoundingBox (V2 innerLeft innerUp, V2 innerRight innerDown)

    degrees = (*) (pi / 180)

    render = do
      Canvas.beginPath
      Canvas.arc innerLeft  innerUp   radius (degrees 180) (degrees 270)
      Canvas.arc innerRight innerUp   radius (degrees 270) (degrees 0)
      Canvas.arc innerRight innerDown radius (degrees 0)   (degrees 90)
      Canvas.arc innerLeft  innerDown radius (degrees 90)  (degrees 180)
      Canvas.closePath


data Path = Path {
  pathStart    :: (Double,Double),
  pathRenderer :: Canvas.Canvas ()
}

renderOpenPath :: Path -> Canvas.Canvas ()
renderOpenPath (Path start renderer) = Canvas.beginPath >> uncurry Canvas.moveTo start >> renderer

renderClosedPath :: Path -> Canvas.Canvas ()
renderClosedPath path = Canvas.beginPath >> renderOpenPath path >> Canvas.closePath


openPath :: Path -> Shape
openPath = Shape . renderOpenPath

closedPath :: Path -> Shape
closedPath = Shape . renderClosedPath


pathPoint :: (Double,Double) -> Path
pathPoint point = Path point (return ())

connectBy :: (Double -> Double -> Canvas.Canvas ())
          -> Path -> Path -> Path
connectBy connector (Path start0 prim0) (Path start1 prim1)
  = Path start0 (prim0 >> connection >> prim1)
  where
    connection = uncurry connector start1

lineConnect :: Path -> Path -> Path
lineConnect = connectBy Canvas.lineTo

curveConnect :: (Double,Double) -> (Double,Double)
             -> Path -> Path -> Path
curveConnect (x1,y1) (x2,y2) = connectBy (Canvas.bezierCurveTo x1 y1 x2 y2)
