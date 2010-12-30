{-# language ViewPatterns #-}

-- | provides a data type for pixmaps that saves the size and the internal offset (padding)
-- of the image.
-- Padding is the outer part of the image that should not be considered as part of 
-- the physical object depictured by the image, e.g. an outer glow.

module Base.Pixmap where


import Data.Abelian

import Control.Monad.IO.Class

import Graphics.Qt

import Utils

import Base.Types


data Pixmap = Pixmap {
    pixmap :: Ptr QPixmap,
    pixmapSize :: Size Double,
    pixmapOffset :: Position Double
  }
    deriving Show

-- | Loads a pixmap. 
loadPixmap :: Position Int -- ^ Size of the padding.
    -> FilePath -> IO Pixmap
loadPixmap padding path = do
    pix <- newQPixmap path
    size <- sizeQPixmap pix
    return $ Pixmap
        pix
        (fmap fromIntegral (size -~ fmap (* 2) (positionToSize padding)))
        (fmap (fromIntegral . negate) padding)

freePixmap :: Pixmap -> IO ()
freePixmap = pixmap >>> destroyQPixmap


-- | renders the pixmap
renderPixmap :: Ptr QPainter -- ^ painter to be rendered to
    -> Offset Double -- ^ global (camera) offset
    -> Position Double -- ^ position of pixmap
    -> Maybe Double -- ^ rotation
    -> Pixmap -- ^ pixmap to be rendered
    -> IO ()
renderPixmap ptr offset position mAngle pix = do
    resetMatrix ptr
    translate ptr offset

    translate ptr position
    whenMaybe mAngle $ \ angle ->
        rotate ptr (rad2deg angle)
    translate ptr (pixmapOffset pix)

    drawPixmap ptr zero (pixmap pix)


renderPixmapSimple :: Ptr QPainter -> Pixmap -> IO ()
renderPixmapSimple ptr pix = do
    drawPixmap ptr (fmap round $ pixmapOffset pix) (pixmap pix)

