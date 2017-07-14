module Graphics.Rendering.Canvas where

import qualified GHCJS.DOM.Types as JS
import qualified GHCJS.DOM.HTMLCanvasElement as JS
import qualified GHCJS.DOM.CanvasRenderingContext2D as JS

import Control.Monad.Reader
import Data.Coerce

type Canvas a = ReaderT JS.CanvasRenderingContext2D JS.JSM a

fillStyle :: String -> Canvas ()
fillStyle style = do
    ctx <- ask
    JS.setFillStyle ctx style

fillRect :: Float -> Float -> Float -> Float -> Canvas ()
fillRect x y w h = do
    ctx <- ask
    JS.fillRect ctx x y w h

runCanvas :: JS.HTMLCanvasElement -> Canvas a -> JS.JSM a
runCanvas canvas action = do
    ctx <- JS.getContextUnchecked canvas "2d" ([] :: [JS.JSString])
    runReaderT action (coerce ctx :: JS.CanvasRenderingContext2D)
