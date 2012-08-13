
module Sorts.Tiles.Baking.Tests where


import Data.List
import Data.Maybe
import Data.Abelian

import Text.Printf

import Control.Arrow
import Control.Exception

import System.FilePath

import Graphics.Qt

import Utils
import Utils.Tests

import Base

import Sorts.Tiles.Baking (bakeTiles, boundingBox)

import Test.QuickCheck
import Test.QuickCheck.Property


tests =
    withQApplication $ \ _qApp ->
    withMainWindow 1 500 500 $ \ _window -> do
        pixmaps <- loadSomePixmaps

        quickCheckWith stdArgs{maxSize = 13, maxSuccess = 500} $
            bakingEquality pixmaps

-- | checks, if baking doesn't affect the rendered image.
bakingEquality :: [Animation Pixmap]
    -> ([Maybe (Position (Fixed Int))], Positive Seconds) -> Property
bakingEquality somePixmaps (positions, Positive now) =
  morallyDubiousIOProperty $ do
    let paired :: [(Animation Pixmap, Position Double)]
        paired =
            fmap (second (fmap fromIntegral)) $
            catMaybes $
            zipWith (\ animation -> fmap (\ position -> (animation, position)))
            somePixmaps $
            fmap (fmap (fmap unFixed)) $
            positions
    expected <- renderToPixmap False now paired
    baked <- renderToPixmap False now =<< bakeTiles paired
    r <- pixelEquality expected baked
    return $ whenFail (saveFailed now paired) r

saveFailed now paired = do
    let save debugMode filename paired = do
            pix <- renderToPixmap debugMode now paired
            saveQPixmap pix filename 100
        saveIter debugMode filenamePattern paired =
            forM_ (zip (tail $ inits paired) [1 :: Int ..]) $ \ (iterPaired, i) ->
                save debugMode (printf filenamePattern i) iterPaired
    save False "A.png" paired
    save False "B.png" =<< bakeTiles paired
--     save True "debugA.png" paired
--     save True "debugB.png" =<< bakeTiles app paired
    saveIter True "debugA-%03i.png" paired
    baked <- bakeTiles paired
    saveIter True "debugB-%03i.png" baked


-- | load some random animated pixmaps (with padding pixels)
loadSomePixmaps :: IO [Animation Pixmap]
loadSomePixmaps = do
    pngs <- fmap (pngDir </>) <$> getFiles pngDir (Just ".png")
    load pngs
  where
    pngDir = (".." </> ".." </> "data" </> "png" </> "objects")

    load :: [FilePath] -> IO [Animation Pixmap]
    load =
        mapM (loadSymmetricPixmap (Position 1 1)) >=>
        return . groupBy (on (==) pixmapSize) >=>
        return . map mkAnim
    mkAnim :: Eq f => [f] -> Animation f
    mkAnim l = mkAnimation l [1]


-- | renders the given pixmaps to a new QPixmap.
renderToPixmap :: Bool -> Seconds -> [(Animation Pixmap, Position Double)]
    -> IO (ForeignPtr QPixmap)
renderToPixmap _ now [] =
    newQPixmapEmpty $ Size 0 0
renderToPixmap debugMode now animations = do
    let pixmaps = map (first (flip pickAnimationFrame now)) animations
        (upperLeft, size) =
            boundingBox $ map (\ (pix, pos) ->
                (pos +~ pix ^. pixmapOffset, pixmapImageSize pix)) pixmaps
    result <- newQPixmapEmpty size
    painter <- newQPainter result
    resetMatrix painter
    translate painter (negateAbelian upperLeft)
    forM_ (zip pixmaps [1 ..]) $ \ ((pix, pos), i) -> do
        let areaPos = pos +~ pix ^. pixmapOffset
            areaSize = pixmapImageSize pix
        drawPixmap painter areaPos (pixmap pix)
        when debugMode $ do
            c <- (alpha ^= 0.4) <$> semiRandomColor
            fillRect painter areaPos areaSize c
    destroyQPainter painter
    return result


-- | Returns if the given QPixmaps have the same content (pixel by pixel)
pixelEquality :: ForeignPtr QPixmap -> ForeignPtr QPixmap -> IO Bool
pixelEquality a b = do
    bracket
        ((,) <$> toImageQPixmap a False <*> toImageQPixmap b False)
        (\ (a, b) -> destroyQImage a >> destroyQImage b)
        (uncurry pixelEqualityQImage)

pixelEqualityQImage :: Ptr QImage -> Ptr QImage -> IO Bool
pixelEqualityQImage a b = do
    aSize@(Size width height) <- sizeQImage a
    bSize <- sizeQImage b
    if aSize /= bSize
      then do
        putStrLn ("sizes not equal: " ++ show (aSize, bSize))
        return False
      else do
        check a b (cartesian [0 .. width - 1] [0 .. height - 1])
  where
    check :: Ptr QImage -> Ptr QImage -> [(Int, Int)] -> IO Bool
    check a b (pos : r) = do
        ap <- qRgbToColor =<< pixelQImage a pos
        bp <- qRgbToColor =<< pixelQImage b pos
        if ap /= bp
          then do
            putStrLn ("pixels not equal at " ++ show pos ++ ": " ++ pp (ap, bp))
            return False
          else check a b r
    check a n [] = return True
