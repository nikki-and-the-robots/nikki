
-- | Showing texts on the screen, till a key is pressed.
-- Used for help screens.

module Base.Renderable.TextListing where


import Graphics.Qt

import Utils

import Base.Types
import Base.Constants
import Base.Polling
import Base.Prose
import Base.Font

import Base.Renderable.Common


drawTextBlock :: Font -> Ptr QPainter -> [Prose] -> IO ()
drawTextBlock font ptr = mapM_ $ \ line -> do
    windowSize <- sizeQPainter ptr
    let wordWrapWidth = width windowSize - (2 * fromUber 4)
    renderSize <- renderLineSimple font (Just wordWrapWidth) standardFontColor line ptr
    translate ptr (Position 0 (height renderSize))

showText :: Application_ sort -> [Prose] -> AppState -> AppState
showText app text follower =
    AppState (rt "showText") $ inner 0
  where
    -- Sets a renderer, that renders the given text with the given scrolling.
    -- Calls itself with another scrolling, if needed.
    inner scrolling = do
        io $ setDrawingCallbackGLContext (window app) $ Just $ render (drop scrolling text)
        e <- waitForPressAppEvent app
        if isDown e then inner (min (succ scrolling) (length text))
         else if isUp e then inner (max (pred scrolling) 0)
         else return follower

    render text ptr = do
        clearScreen ptr backgroundColor
        resetMatrix ptr
        translate ptr (Position (fromUber 4) (fromUber 4))
        let font = alphaNumericFont $ applicationPixmaps app
        drawTextBlock font ptr text
