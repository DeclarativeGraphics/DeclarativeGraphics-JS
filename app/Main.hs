{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import GHCJS.DOM
import GHCJS.DOM.Types
import GHCJS.DOM.Document
import GHCJS.DOM.Element
import GHCJS.DOM.Node

import Graphics.Rendering.Canvas

main :: IO ()
main = liftJSM helloMain

helloMain :: JSM ()
helloMain = do
    doc <- currentDocumentUnchecked
    body <- getBodyUnchecked doc

    canvas <- uncheckedCastTo HTMLCanvasElement <$> createElement doc "canvas"

    appendChild_ body canvas

    runCanvas canvas render

render :: Canvas ()
render = do
  fillStyle "rgb(200, 0, 0)"
  fillRect 10 10 90 50

  fillStyle "rgba(0, 0, 200, 0.5)"
  fillRect 30 30 50 50

  fillText 10 90 "Hello, GHCJS!"

  beginPath
  moveTo 60 10
  lineTo 60 70
  lineTo 70 70
  closePath
  stroke
