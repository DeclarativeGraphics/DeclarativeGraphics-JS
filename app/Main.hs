{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import GHCJS.DOM
import GHCJS.DOM.Types
import GHCJS.DOM.Document
import GHCJS.DOM.Element
import GHCJS.DOM.Node

import Graphics.Rendering.Canvas

import Graphics.Declarative.Canvas.Shape as Shape
import Graphics.Declarative.Bordered

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
  beginPath
  rect 10 10 90 50
  fill

  fillStyle "rgba(0, 0, 200, 0.5)"
  beginPath
  rect 30 30 50 50
  fill

  fillText 10 90 "Hello, GHCJS!"

  Shape.renderShape $ Shape.closedPath (foldr1 Shape.lineConnect (map Shape.pathPoint path))
  stroke

path :: [(Double, Double)]
path = [(120, 10), (120, 40), (150, 30)]
