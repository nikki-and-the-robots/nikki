{-# language ScopedTypeVariables, TypeSynonymInstances #-}

-- | Things used in multiple Widgets.

module Base.Renderable.Common where


import Data.Abelian

import System.FilePath

import Graphics.Qt

import Utils

import Base.Constants
import Base.Types
import Base.Polling
import Base.Pixmap


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
    fmap ((/ 4) >>> round >>> fromIntegral >>> (* 4))

-- * Renderable

rt :: String -> RenderableInstance
rt = RenderableInstance

-- NYI instance
instance Renderable String where
    render _ _ _ msg = tuple (Size 10 10) $ putStrLn ("NYI: renderable " ++ msg)

instance Renderable Pixmap where
    render ptr app size pix = tuple (pixmapSize pix) $ do
        translate ptr (pix ^. pixmapOffset)
        drawPixmap ptr zero (pixmap pix)
