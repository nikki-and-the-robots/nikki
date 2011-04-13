{-# language ScopedTypeVariables #-}

module Base.Application.Widgets.Message (message) where


import Data.Abelian

import Graphics.Qt

import Utils

import Base.Types hiding (render)
import Base.Prose
import Base.Font
import Base.Polling

import Base.Application.Widgets.Common


-- | show a textual message and wait for a keypress
message :: Application_ sort -> [Prose] -> M ()
message app texts = do
    io $ setDrawingCallbackGLContext (window app) (Just $ render app texts)
    void $ waitForPressAppEvent app

render :: Application_ sort -> [Prose] -> Ptr QPainter -> IO ()
render app texts ptr = do
    clearScreen ptr darkGrey
    let font = alphaNumericFont $ applicationPixmaps app
    windowSize <- sizeQPainter ptr
    resetMatrix ptr
    forM_ texts $ \ text -> do
        translate ptr (Position 0 (fontHeight font))
        renderCentered font windowSize text
    translate ptr (Position 0 (fontHeight font * 3))
    renderCentered font windowSize (p "press any key to continue")
  where
    renderCentered font windowSize text = do
        (renderAction, textSize) <- renderLine font (Just (width windowSize)) standardFontColor text
        let centerX :: Double = width (fmap fromIntegral windowSize -~ textSize) / 2
        translate ptr (Position centerX 0)
        renderAction ptr
        translate ptr (Position (- centerX) 0)
