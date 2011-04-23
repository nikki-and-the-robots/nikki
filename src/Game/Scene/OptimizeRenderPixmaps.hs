{-# language ViewPatterns #-}

module Game.Scene.OptimizeRenderPixmaps (optimize) where


import Data.Abelian

import Graphics.Qt

import Utils

import Base


-- | removes the RenderPixmaps that wouldn't be visible on screen
optimize :: Size Double -> [RenderPixmap] -> [RenderPixmap]
optimize windowSize = filter (inView windowSize)

inView windowSize (RenderPixmap pix objectPosition Nothing) =
    let picturePosition@(Position x y) = objectPosition -~ pix ^. pixmapOffset
        size = pixmapImageSize pix
        xGreater = x + width size >= 0
        xLesser = x <= width windowSize
        yGreater = y + height size >= 0
        yLesser = y <= height windowSize
    in (xGreater && xLesser && yGreater && yLesser)
inView _ _ = True
