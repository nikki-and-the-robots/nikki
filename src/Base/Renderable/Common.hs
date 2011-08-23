{-# language ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances #-}

-- | Things used in multiple Widgets.

module Base.Renderable.Common where


import Data.Abelian

import Graphics.Qt

import Utils

import Base.Constants
import Base.Configuration
import Base.Types
import Base.Polling
import Base.Pixmap
import Base.Prose
import Base.Font


-- | Blocks until a Press AppEvent is received.
-- Flushes the event queue before that.
waitForPressedButton :: Application -> M Button
waitForPressedButton app =
    waitForSpecialPressedButton app (const True)

-- | Blocks until a Press AppEvent that satisfies the given property
-- is received.
-- Flushes the event queue before that.
waitForSpecialPressedButton :: Application -> (Button -> Bool) -> M Button
waitForSpecialPressedButton app property = do
    inner
  where
    inner = do
        e <- waitForAppEvent app
        case e of
            (Press b) | property b -> return b
            _ -> inner


-- * Renderable

instance Renderable RenderableInstance where
    render ptr app config size (RenderableInstance r) =
        (if show_widget_frames config
            then fiddleInDebugging ptr r
            else id) <$>
        render ptr app config size r
    label (RenderableInstance r) = label r

fiddleInDebugging ptr renderable (widgetSize, action) =
    (widgetSize, recoverMatrix ptr action >> debugRender)
  where
    debugRender = do
        color <- alpha ^= 1 <$> randomColor
        setPenColor ptr color 1
        drawRect ptr zero widgetSize
        fillRect ptr zero widgetSize (alpha ^= 0.1 $ color)
        drawText ptr (Position 10 25) False (head $ words $ label renderable)

instance Renderable Pixmap where
    render ptr app _ size pix = return $ tuple (pixmapSize pix) $ do
        translate ptr (pix ^. pixmapOffset)
        drawPixmap ptr zero (pixmap pix)
    label = const "Pixmap"

-- | used for rendering one line of text
-- (all other text rendering is implemented in terms of this)
instance Renderable [Glyph] where
    render ptr app config parentSize [] = return (zero, return ())
    render ptr app config parentSize glyphs =
        return (size, action)
      where
        size = Size
            ((sum $ fmap (width . glyphSize) glyphs) + kerning)
            fontHeight
        kerning = fromUber (fromIntegral (length glyphs) - 1)
        action = forM_ glyphs $ \ glyph -> do
            recoverMatrix ptr $ do
                -- center glyphs in fontheight horizontally
                translate ptr (Position 0 ((fontHeight - height (glyphSize glyph)) / 2))
                (snd =<< render ptr app config size (glyphPixmap glyph))
            translate ptr (Position (width (glyphSize glyph) + fromUber 1) 0)
    label = const "[Glyph]"

-- | text rendering without word wrapping
instance Renderable Prose where
    render ptr app config size prose =
        render ptr app config size $ proseToGlyphs (standardFont app) prose
    label = const "Prose"
