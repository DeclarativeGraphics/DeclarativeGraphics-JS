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
import GHCJS.DOM.HTMLHyperlinkElementUtils


main :: IO ()
main = liftDOM helloMain

helloMain :: JSM ()
helloMain = do
    doc <- currentDocumentUnchecked
    body <- getBodyUnchecked doc
    setInnerHTML body "<h1>Kia ora (Hi)</h1>"

    -- Add a mouse click event handler to the document
    _ <- on doc click $ do
        (x, y) <- mouseClientXY
        newParagraph <- uncheckedCastTo HTMLParagraphElement <$> createElement doc "p"
        text <- createTextNode doc $ "Click " ++ show (x, y)
        appendChild_ newParagraph text
        appendChild_ body newParagraph

    -- Make an exit link
    exitMVar <- liftIO newEmptyMVar
    exit <- uncheckedCastTo HTMLAnchorElement <$> createElement doc "a"
    text <- createTextNode doc "Click here to exit"
    appendChild_ exit text
    appendChild_ body exit

    -- Set an href for the link, but use preventDefault to stop it working
    -- (demonstraights synchronous callbacks into haskell, as preventDefault
    -- must be called inside the JavaScript event handler function).
    setHref exit "https://github.com/ghcjs/ghcjs-dom-hello"
    _ <- on exit click $ preventDefault >> liftIO (putMVar exitMVar ())

    -- Force all all the lazy JSaddle evaluation to be executed
    syncPoint

    -- Wait until the user clicks exit.
    liftIO $ takeMVar exitMVar
    setInnerHTML body "<h1>Ka kite ano (See you later)</h1>"
