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
        on (>) width pixmapSize windowSize &&
        on (>) height pixmapSize windowSize

renderWholeScreenPixmap :: Ptr QPainter -> Size Double -> [Pixmap] -> IO ()
renderWholeScreenPixmap ptr size pixmaps = do
    let mPix = pickWholeScreenPixmap size pixmaps
        pix = fromMaybe (last pixmaps) mPix
        offset = size2position $ fmap (/ 2) (size -~ pixmapSize pix)
    when (isNothing mPix) $
        -- background image is too small
        clearScreen ptr black
    drawPixmap ptr offset (pixmap pix)


-- * Renderable

data WholeScreenPixmap
    = MenuBackground
    | MenuBackgroundTransparent
  deriving Show

instance Renderable WholeScreenPixmap where
    render ptr app _config size typ = return $ tuple size $
        renderWholeScreenPixmap ptr size (typPixmaps app typ)
    label = show

typPixmaps :: Application -> WholeScreenPixmap -> [Pixmap]
typPixmaps app MenuBackground =
    menuBackground $ applicationPixmaps app
typPixmaps app MenuBackgroundTransparent =
    menuBackgroundTransparent $ applicationPixmaps app
