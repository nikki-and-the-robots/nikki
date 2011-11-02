{-# language FlexibleInstances, DeriveDataTypeable #-}

-- | provides a data type for pixmaps that saves the size and the internal offset (padding)
-- of the image.
-- Padding is the outer part of the image that should not be considered as part of 
-- the physical object depictured by the image, e.g. an outer glow.

module Base.Pixmap (
    Offset,
    Pixmap(..),
    pixmapOffset,
    loadPixmap,
    loadSymmetricPixmap,
    freePixmap,
    copyPixmap,
    mapColors,
    renderPixmapSimple,
    renderPixmap,
    RenderPixmap(RenderPixmap, RenderCommand, RenderOnTop),
    renderPosition,
    doRenderPixmaps,
  ) where


import Data.Abelian
import Data.Data
import Data.Accessor
import Data.Maybe

import Control.Arrow
import Control.Monad.IO.Class

import Graphics.Qt
import Physics.Chipmunk (Angle, rad2deg)

import Utils


type Offset a = Position a

data Pixmap = Pixmap {
    pixmap :: Ptr QPixmap,
    pixmapOffset_ :: Position Double,
    pixmapSize :: Size Double,
    pixmapImageSize :: Size Double
  }
    deriving (Show, Eq, Typeable, Data)

pixmapOffset :: Accessor Pixmap (Position Double)
pixmapOffset = accessor pixmapOffset_ (\ a r -> r{pixmapOffset_ = a})


-- | Loads a pixmap with a symmetric offset (right == left && above == below).
loadSymmetricPixmap :: MonadIO m =>
    Position Double -- ^ Offset of the object in the image
    -> FilePath -> m Pixmap
loadSymmetricPixmap padding path = io $ do
    pix <- newQPixmap path
    size <- fmap fromIntegral <$> sizeQPixmap pix
    return $ Pixmap
        pix
        (fmap negate padding)
        (size -~ fmap (* 2) (position2size padding))
        size

-- | Loads a pixmap.
-- The offset and size define the offset and size of the object in the picture.
loadPixmap :: MonadIO m => Offset Double -> Size Double -> FilePath -> m Pixmap
loadPixmap offset size file = io $ do
    pixmap <- newQPixmap file
    imageSize <- fmap fromIntegral <$> sizeQPixmap pixmap
    return $ Pixmap pixmap (fmap negate offset) size imageSize

-- | release the resource
freePixmap :: MonadIO m => Pixmap -> m ()
freePixmap = pixmap >>> io . destroyQPixmap

-- | copy a pixmap
copyPixmap :: Pixmap -> IO Pixmap
copyPixmap (Pixmap pix size offset imageSize) = do
    pixCopy <- copyQPixmap pix
    return $ Pixmap pixCopy size offset imageSize

-- | Iterates over the colors in the color table of the image.
mapColors :: (QRgb -> QRgb) -> Pixmap -> IO Pixmap
mapColors f (Pixmap pix size offset realSize) = do
    image <- toImageQPixmap pix
    destroyQPixmap pix
    imageSize <- sizeQImage image

    colorN <- colorCountQImage image
    forM_ [0 .. pred colorN] $ \ i ->
        setColorQImage image i . f =<< colorQImage image i

    newPix <- fromImageQPixmap image
    destroyQImage image
    return $ Pixmap newPix size offset realSize


-- * rendering

-- | renders the pixmap
renderPixmap :: MonadIO m =>
    Ptr QPainter -- ^ painter to be rendered to
    -> Offset Double -- ^ global (camera) offset
    -> Position Double -- ^ position of pixmap
    -> Maybe Angle -- ^ rotation
    -> Pixmap -- ^ pixmap to be rendered
    -> m ()
renderPixmap ptr offset position mAngle pix = io $ do
    resetMatrix ptr
    translate ptr offset

    translate ptr position
    whenMaybe mAngle $ \ angle ->
        rotate ptr (rad2deg angle)
    translate ptr (pix ^. pixmapOffset)

    drawPixmap ptr zero (pixmap pix)

-- | renders a Pixmap without altering the painter matrix
renderPixmapSimple :: MonadIO m => Ptr QPainter -> Pixmap -> m ()
renderPixmapSimple ptr pix = io $
    drawPixmap ptr (pix ^. pixmapOffset) (pixmap pix)

-- | pixmap with rendering information (position and angle)
data RenderPixmap
    = RenderPixmap {
        getRenderPixmap :: Pixmap,
        renderPosition_ :: Position Double,
        renderAngle :: Maybe Angle
      }
    | RenderCommand {
        renderPosition_ :: Position Double,
        renderCommand :: (Ptr QPainter -> IO ())
      }
    | RenderOnTop { -- to be rendered on top. (After all other RenderPixmaps)
        renderInnerPixmap :: RenderPixmap
      }
  deriving (Show, Typeable, Data)

instance Show (Ptr QPainter -> IO ()) where
    show = const "<Ptr QPainter -> IO ()>"

-- | Removes RenderOnTops and enqueues them at the end.
removeOnTops :: [RenderPixmap] -> [RenderPixmap]
removeOnTops =
    inner []
  where
    inner onTops (RenderOnTop i : r) = inner (i : onTops) r
    inner onTops (a : r) = a : inner onTops r
    inner [] [] = []
    inner onTops [] = inner [] (reverse onTops)

renderPosition :: Accessor RenderPixmap (Position Double)
renderPosition = accessor getter setter
  where
    getter (RenderPixmap _ p _) = p
    getter (RenderCommand p _) = p
    getter (RenderOnTop inner) = getter inner
    setter p (RenderPixmap a _ c) = RenderPixmap a p c
    setter p (RenderCommand _ a) = RenderCommand p a
    setter p (RenderOnTop inner) = RenderOnTop (setter p inner)

-- | renders a list of RenderPixmaps. Renders the top layers after that.
doRenderPixmaps :: Ptr QPainter -> [RenderPixmap] -> IO ()
doRenderPixmaps ptr pixmaps = do
    let groupedPixmaps = groupRenderPixmaps $ removeOnTops pixmaps
--     putStrLn $ statistics groupedPixmaps
    fmapM_ (renderGroupedPixmap ptr) groupedPixmaps

-- | type for internally storing Pixmaps for optimizing rendering the same pixmaps
-- multiple times
data GroupedPixmap
    = GroupedPixmaps (Ptr QPixmap) [(Position Double, Angle)]
    | GroupedCommand (Position Double) (Ptr QPainter -> IO ())

groupRenderPixmaps :: [RenderPixmap] -> [GroupedPixmap]
groupRenderPixmaps ((RenderPixmap pix pos mAngle) : r) =
    GroupedPixmaps (pixmap pix) fragments : groupRenderPixmaps r'
  where
    fragments = toFragment pix pos mAngle : bs
    (bs, r') = samePixmaps pix r

    samePixmaps :: Pixmap -> [RenderPixmap]
        -> ([(Position Double, Angle)], [RenderPixmap])
    samePixmaps searched l@(RenderPixmap pix pos mAngle : r) =
        if pixmap pix == pixmap searched
        then first (toFragment pix pos mAngle :) $ samePixmaps searched r
        else ([], l)
    samePixmaps searched r = ([], r)

groupRenderPixmaps (RenderCommand pos command : r) =
    GroupedCommand pos command : groupRenderPixmaps r
groupRenderPixmaps [] = []

toFragment :: Pixmap -> Position Double -> Maybe Angle -> (Position Double, Angle)
toFragment pix position mAngle =
    (center, rad2deg angle)
  where
    angle = fromMaybe 0 mAngle
    center = position
            +~ rotatePosition angle (fmap (/ 2) $ size2position (pixmapImageSize pix))
            +~ rotatePosition angle (pix ^. pixmapOffset)

statistics :: [GroupedPixmap] -> String
statistics l =
    "grouped pixmaps: " ++ show (inner l)
  where
    inner (GroupedPixmaps _ l : r) = length l : inner r
    inner (GroupedCommand{} : r) = inner r
    inner [] = []

renderGroupedPixmap :: Ptr QPainter -> GroupedPixmap -> IO ()
renderGroupedPixmap ptr (GroupedPixmaps pix fragments) =
    drawPixmapFragments ptr fragments pix
renderGroupedPixmap ptr (GroupedCommand pos command) = do
    resetMatrix ptr
    translate ptr pos
    command ptr

-- old implementation (not used)
-- doRenderPixmap ptr (RenderPixmap pix position mAngle) = do
--     resetMatrix ptr
--     translate ptr position
--     whenMaybe mAngle $ \ angle ->
--         rotate ptr (rad2deg angle)
--     translate ptr (pix ^. pixmapOffset)
-- 
--     drawPixmap ptr zero (pixmap pix)
--     return Nothing
