{-# language ScopedTypeVariables, TypeSynonymInstances #-}

-- | Things used in multiple Widgets.

module Base.Renderable.Common where


import Data.Abelian

import Control.Arrow

import System.FilePath

import Graphics.Qt

import Utils

import Base.Constants
import Base.Configuration
import Base.Types
import Base.Polling
import Base.Pixmap
import Base.Prose


backgroundColor :: Color = QtColor 16 0 156 255
standardFontColor :: Color = QtColor 70 210 245 255

menuDir :: FilePath
menuDir = pngDir </> "menu"

-- | Blocks until a Press AppEvent is received.
-- Flushes the event queue before that.
waitForPressButton :: Application_ s -> M Button
waitForPressButton app = do
    inner
  where
    inner = do
        e <- waitForAppEvent app
        case e of
            (Press b) -> return b
            _ -> inner


-- | rounds to uberpixels
uberRound :: Position Double -> Position Double
uberRound =
    fmap ((/ 4) >>> floor >>> fromIntegral >>> (* 4))

-- * Renderable

instance Renderable RenderableInstance where
    render ptr app config size (RenderableInstance r) =
        (if show_widget_frames config
            then fiddleInDebugging ptr r
            else id) <$>
        render ptr app config size r

fiddleInDebugging ptr renderable (widgetSize, action) =
    (widgetSize, recoverMatrix ptr action >> debugRender)
  where
    debugRender = do
        color <- alpha ^= 1 <$> randomColor
        setPenColor ptr color 1
        drawRect ptr zero widgetSize
        eraseRect ptr zero (fmap round widgetSize) (alpha ^= 0.1 $ color)
        drawText ptr (Position 10 25) False (head $ words $ show renderable)

rt :: String -> RenderableInstance
rt = RenderableInstance

-- NYI instance
instance Renderable String where
    render _ _ _ _ msg = return $ tuple (Size 10 10) $ putStrLn ("NYI: renderable " ++ msg)

instance Renderable Pixmap where
    render ptr app _ size pix = return $ tuple (pixmapSize pix) $ do
        translate ptr (pix ^. pixmapOffset)
        drawPixmap ptr zero (pixmap pix)
