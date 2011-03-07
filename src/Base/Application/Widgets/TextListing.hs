
-- | Showing texts on the screen, till a key is pressed.
-- Used for help screens.

module Base.Application.Widgets.TextListing where


import Graphics.Qt

import Utils

import Base.Types
import Base.Constants
import Base.Polling
import Base.Prose
import Base.Font


drawTextBlock :: Font -> Ptr QPainter -> [Prose] -> IO ()
drawTextBlock font ptr = mapM_ $ \ line -> do
    windowSize <- sizeQPainter ptr
    let wordWrapWidth = width windowSize - (2 * fromUber 4)
    renderSize <- renderLineSimple font (Just wordWrapWidth) standardFontColor line ptr
    translate ptr (Position 0 (height renderSize))

showText :: Application_ sort -> [Prose] -> AppState -> AppState
showText app text follower =
    AppState $ inner 0
  where
    -- Sets a renderer, that renders the given text with the given scrolling.
    -- Calls itself with another scrolling, if needed.
    inner scrolling = do
        io $ setDrawingCallbackGLContext (window app) $ Just $ render (drop scrolling text)
        e <- waitForAppEvent app $ keyPoller app
        case e of
            Press e | isDown e -> inner (min (succ scrolling) (length text))
            Press e | isUp e -> inner (max (pred scrolling) 0)
            Press _ -> return follower
            _ -> inner scrolling

    render text ptr = do
        clearScreen ptr darkGrey
        resetMatrix ptr
        translate ptr (Position (fromUber 4) (fromUber 4))
        let font = alphaNumericFont $ applicationPixmaps app
        drawTextBlock font ptr text
