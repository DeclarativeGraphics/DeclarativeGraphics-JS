{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import GHCJS.DOM
import GHCJS.DOM.Types
import GHCJS.DOM.Document
import GHCJS.DOM.Element
import GHCJS.DOM.Node

import Graphics.Rendering.Canvas

import Data.Coerce

main :: IO ()
main = liftJSM helloMain

helloMain :: JSM ()
helloMain = do
    doc <- currentDocumentUnchecked
    body <- getBodyUnchecked doc

    (canvas :: HTMLCanvasElement) <- coerce <$> createElement doc "canvas"

    appendChild_ body canvas

    runCanvas canvas render

render :: Canvas ()
render = do
  fillStyle "rgb(200, 0, 0)"
  fillRect 10 10 90 50

  fillStyle "rgba(0, 0, 200, 0.5)"
  fillRect 30 30 50 50
