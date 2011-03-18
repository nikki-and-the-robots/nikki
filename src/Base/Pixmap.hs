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


-- | Loads a pixmap. 
loadPixmap :: MonadIO m => Position Int -- ^ Size of the padding.
    -> FilePath -> m Pixmap
loadPixmap padding path = io $ do
    pix <- newQPixmap path
    size <- sizeQPixmap pix
    return $ Pixmap
        pix
        (fmap fromIntegral (size -~ fmap (* 2) (positionToSize padding)))
        (fmap (fromIntegral . negate) padding)

-- | release the resource
freePixmap :: MonadIO m => Pixmap -> m ()
freePixmap = pixmap >>> io . destroyQPixmap

-- | copy a pixmap
copyPixmap :: Pixmap -> IO Pixmap
copyPixmap (Pixmap pix size offset) = do
    pixCopy <- copyQPixmap pix
    return $ Pixmap pixCopy size offset

-- | Change the pixel colors of a given pixmap using the given function.
-- Not very efficient, since the Pixmap is converted to a QImage in between.
mapPixels :: (QRgb -> QRgb) -> Pixmap -> IO Pixmap
mapPixels f (Pixmap pix size offset) = do
    image <- toImageQPixmap pix
    destroyQPixmap pix
    imageSize <- sizeQImage image
    let xs = [0 .. (width imageSize - 1)]
        ys = [0 .. (height imageSize - 1)]
    forM_ (cartesian ys xs)  $ \ (y, x) -> do
        c <- pixelQImage image (x, y)
        setPixelQImage image (x, y) (f c)
    newPix <- fromImageQPixmap image
    destroyQImage image
    return $ Pixmap newPix size offset

-- | renders the pixmap
renderPixmap :: MonadIO m =>
    Ptr QPainter -- ^ painter to be rendered to
    -> Offset Double -- ^ global (camera) offset
    -> Position Double -- ^ position of pixmap
    -> Maybe Double -- ^ rotation
    -> Pixmap -- ^ pixmap to be rendered
    -> m ()
renderPixmap ptr offset position mAngle pix = io $ do
    resetMatrix ptr
    translate ptr offset

    translate ptr position
    whenMaybe mAngle $ \ angle ->
        rotate ptr (rad2deg angle)
    translate ptr (pixmapOffset pix)

    drawPixmap ptr zero (pixmap pix)

-- | renders a Pixmap without altering the painter matrix
renderPixmapSimple :: MonadIO m => Ptr QPainter -> Pixmap -> m ()
renderPixmapSimple ptr pix = io $
    drawPixmap ptr (fmap round $ pixmapOffset pix) (pixmap pix)
