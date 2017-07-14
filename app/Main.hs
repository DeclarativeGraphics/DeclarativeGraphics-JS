{-# LANGUAGE ScopedTypeVariables #-}
module Main where


import Control.Monad.IO.Class (MonadIO(..))
import Control.Concurrent.MVar (takeMVar, putMVar, newEmptyMVar)

import GHCJS.DOM
import GHCJS.DOM.Types
import GHCJS.DOM.Document
import GHCJS.DOM.Element
import GHCJS.DOM.Node
import GHCJS.DOM.EventM
import GHCJS.DOM.GlobalEventHandlers
import GHCJS.DOM.HTMLCanvasElement
import GHCJS.DOM.CanvasRenderingContext2D

import Data.Coerce

main :: IO ()
main = liftDOM helloMain

helloMain :: JSM ()
helloMain = do
    doc <- currentDocumentUnchecked
    body <- getBodyUnchecked doc

    (canvas :: HTMLCanvasElement) <- coerce <$> createElement doc "canvas"

    appendChild_ body canvas

    (ctx :: CanvasRenderingContext2D) <- coerce <$> getContextUnchecked canvas "2d" ([] :: [JSString])

    setFillStyle ctx "rgb(200, 0, 0)"
    fillRect ctx 10 10 50 50

    setFillStyle ctx "rgba(0, 0, 200, 0.5)"
    fillRect ctx 30 30 50 50

    return ()
