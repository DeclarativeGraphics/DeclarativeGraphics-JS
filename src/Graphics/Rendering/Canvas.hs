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

fillStyle :: String -> Canvas ()
fillStyle style = do
    ctx <- ask
    JS.setFillStyle ctx style

fillRect :: Float -> Float -> Float -> Float -> Canvas ()
fillRect x y w h = do
    ctx <- ask
    JS.fillRect ctx x y w h

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

moveTo :: Double -> Double -> Canvas ()
moveTo x y = do
    ctx <- ask
    JS.moveTo ctx x y

lineTo :: Double -> Double -> Canvas ()
lineTo x y = do
    ctx <- ask
    JS.lineTo ctx x y
