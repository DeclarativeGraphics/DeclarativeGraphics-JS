module Graphics.Rendering.Canvas where

import qualified GHCJS.DOM.Types as JS
import qualified GHCJS.DOM.HTMLCanvasElement as JS
import qualified GHCJS.DOM.CanvasRenderingContext2D as JS
import qualified GHCJS.DOM.CanvasPath as JS

import Control.Monad.Reader
import Data.Coerce

type Canvas a = ReaderT JS.CanvasRenderingContext2D JS.JSM a

runCanvas :: JS.HTMLCanvasElement -> Canvas a -> JS.JSM a
runCanvas canvas action = do
    ctx <- JS.getContextUnchecked canvas "2d" ([] :: [JS.JSString])
    runReaderT action (coerce ctx :: JS.CanvasRenderingContext2D)

save :: Canvas ()
save = JS.save =<< ask

restore :: Canvas ()
restore = JS.restore =<< ask

fillStyle :: String -> Canvas ()
fillStyle style = do
    ctx <- ask
    JS.setFillStyle ctx style

rect :: Double -> Double -> Double -> Double -> Canvas ()
rect x y w h = do
    ctx <- ask
    JS.rect ctx x y w h

fillText :: Float -> Float -> String -> Canvas ()
fillText x y text = do
    ctx <- ask
    JS.fillText ctx text x y Nothing

beginPath :: Canvas ()
beginPath = JS.beginPath =<< ask

closePath :: Canvas ()
closePath = JS.closePath =<< ask

stroke :: Canvas ()
stroke = JS.stroke =<< ask

fill :: Canvas ()
fill = do
    ctx <- ask
    JS.fill ctx Nothing

moveTo :: Double -> Double -> Canvas ()
moveTo x y = do
    ctx <- ask
    JS.moveTo ctx x y

lineTo :: Double -> Double -> Canvas ()
lineTo x y = do
    ctx <- ask
    JS.lineTo ctx x y

arc :: Double -> Double -> Double -> Double -> Double -> Canvas ()
arc xc yc radius angle1 angle2 = do
    ctx <- ask
    JS.arc ctx xc yc radius angle1 angle2 False

quadraticCurveTo :: Double -> Double -> Double -> Double -> Canvas ()
quadraticCurveTo x1 y1 x2 y2 = do
    ctx <- ask
    JS.quadraticCurveTo ctx x1 y1 x2 y2

bezierCurveTo :: Double -> Double -> Double -> Double -> Double -> Double -> Canvas ()
bezierCurveTo x1 y1 x2 y2 x3 y3 = do
    ctx <- ask
    JS.bezierCurveTo ctx x1 y1 x2 y2 x3 y3
