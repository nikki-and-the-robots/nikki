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
import Data.IORef
import Data.Map (Map, empty, insert, lookup)

import Control.Monad.IO.Class
import Control.DeepSeq

import System.IO.Unsafe

import Foreign.Ptr (nullPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)

import Graphics.Qt
import Physics.Chipmunk (Angle, rad2deg)

import Utils


type Offset a = Position a

data Pixmap = Pixmap {
    pixmap :: ForeignPtr QPixmap,
    pixmapOffset_ :: Position Double,
    pixmapSize :: Size Double,
    pixmapImageSize :: Size Double
  }
    deriving (Show, Eq, Typeable, Data)

instance NFData (ForeignPtr a) where
    rnf ptr = unsafeForeignPtrToPtr ptr == nullPtr `seq` ()

instance NFData Pixmap where
    rnf (Pixmap a b c d) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d

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
    image <- toImageQPixmap pix True
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
    onTop <- catMaybes <$> fmapM (doRenderPixmap ptr) pixmaps
    when (not $ null onTop) $
        doRenderPixmaps ptr onTop

-- | renders a pixmap and returns the layer to be rendered on top of that
doRenderPixmap :: Ptr QPainter -> RenderPixmap -> IO (Maybe RenderPixmap)
doRenderPixmap ptr (RenderPixmap pix position mAngle) = do
    resetMatrix ptr
    translate ptr position
    whenMaybe mAngle $ \ angle ->
        rotate ptr (rad2deg angle)
    translate ptr (pix ^. pixmapOffset)

    drawPixmap ptr zero (pixmap pix)
    when debugMode $ debugPixmaps ptr pix position mAngle
    return Nothing
doRenderPixmap ptr (RenderCommand position command) = do
    resetMatrix ptr
    translate ptr position
    command ptr
    return Nothing
doRenderPixmap ptr r@(RenderOnTop x) = do
    return $ Just x


-- * debugging

debugMode = False

debugPixmaps :: Ptr QPainter -> Pixmap -> Position Double -> Maybe Angle -> IO ()
-- rotated Pixmaps aren't displayed
debugPixmaps ptr pix position (Just _) = return ()
debugPixmaps ptr pix position Nothing = do
    key <- uniqueKey (pixmap pix)
    let upperLeft = position +~ (pix ^. pixmapOffset)
    resetMatrix ptr
    fillRect ptr upperLeft (pixmapImageSize pix) (alpha ^= 0.2 $ red)
    drawText ptr upperLeft True key

-- | Returns a unique, preferrebly short key for the given argument.
-- (Unique per run of the program.)
uniqueKey :: ForeignPtr QPixmap -> IO String
uniqueKey x = do
    (map, newKey : restKeys) <- readIORef _uniqueKeys
    case Data.Map.lookup x map of
        Just x -> return x
        Nothing -> do
            writeIORef _uniqueKeys (insert x (show newKey) map, restKeys)
            return (show newKey)

{-# noinline _uniqueKeys #-}
_uniqueKeys :: IORef (Map (ForeignPtr QPixmap) String, [Int])
_uniqueKeys = unsafePerformIO $ newIORef (empty, [0 ..])

