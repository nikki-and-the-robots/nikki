{-# language ViewPatterns #-}

module Game.Scene.OptimizeRenderPixmaps (optimize) where


import Data.Abelian

import Control.Monad

import Graphics.Qt

import Utils

import Base


-- | removes the RenderPixmaps that wouldn't be visible on screen
optimize :: Size Double -> [RenderPixmap] -> IO [RenderPixmap]
optimize windowSize = filterM (inView windowSize)

inView windowSize (RenderPixmap pix objectPosition Nothing) = 
    sizeQPixmap (pixmap pix) <>> \ (fmap fromIntegral -> size) ->
        let picturePosition@(Position x y) = objectPosition -~ pixmapOffset pix
            xGreater = x + width size >= 0
            xLesser = x <= width windowSize
            yGreater = y + height size >= 0
            yLesser = y <= height windowSize
        in (xGreater && xLesser && yGreater && yLesser)
inView _ _ = return True
