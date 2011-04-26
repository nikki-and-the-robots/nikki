{-# language ViewPatterns #-}

module Base.Renderable.WholeScreenPixmap where


import Safe

import Data.Maybe
import Data.Abelian

import Graphics.Qt

import Utils

import Base.Pixmap
import Base.Types


-- | returns the smallest pixmap that can cover the whole window, if it exists
pickWholeScreenPixmap :: Size Double -> [Pixmap] -> Maybe Pixmap
pickWholeScreenPixmap windowSize list =
    headMay $ filter isBiggerThanWindow list
  where
    isBiggerThanWindow (pixmapSize -> pixmapSize) =
        withView width (>) pixmapSize windowSize &&
        withView height (>) pixmapSize windowSize

renderWholeScreenPixmap :: Ptr QPainter -> Size Double -> [Pixmap] -> IO ()
renderWholeScreenPixmap ptr size pixmaps = do
    let mPix = pickWholeScreenPixmap size pixmaps
        pix = fromMaybe (last pixmaps) mPix
        offset = sizeToPosition $ fmap (round . (/ 2)) (size -~ pixmapSize pix)
    when (isNothing mPix) $
        -- background image is too small
        clearScreen ptr black
    drawPixmap ptr offset (pixmap pix)


-- * Renderable

data WholeScreenPixmap
    = MenuBackground
  deriving Show

instance Renderable WholeScreenPixmap where
    render ptr app size typ = tuple size $
        renderWholeScreenPixmap ptr size (typPixmaps app typ)

typPixmaps :: Application_ s -> WholeScreenPixmap -> [Pixmap]
typPixmaps app MenuBackground = menuBackgrounds $ applicationPixmaps app
