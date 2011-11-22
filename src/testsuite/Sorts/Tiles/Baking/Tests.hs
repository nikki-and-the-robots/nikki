
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
import Test.QuickCheck.Store


tests =
    withQApplication $ \ qApp ->
    withMainWindow 1 500 500 $ \ window -> do
    withSomePixmaps $ \ pixmaps -> do
        let app = Application qApp window err err err err err err
            err = error "uninitialised field in Application: Sorts.Tiles.Baking.Tests"

        quickCheckStoreWith stdArgs{maxSize = 13, maxSuccess = 500} "bakingEquality" $
            bakingEquality app pixmaps

-- | checks, if baking doesn't affect the rendered image.
bakingEquality :: Application -> [Animation Pixmap]
    -> ([Maybe (Position (Fixed Int))], Positive Seconds) -> Property
bakingEquality app somePixmaps (positions, Positive now) =
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
    baked <- renderToPixmap False now =<< bakeTiles app paired
    r <- pixelEquality expected baked
    destroyQPixmap expected
    destroyQPixmap baked
    return $ whenFail (saveFailed app now paired) r

saveFailed app now paired = do
    let save debugMode filename paired = do
            pix <- renderToPixmap debugMode now paired
            saveQPixmap pix filename 100
            destroyQPixmap pix
        saveIter debugMode filenamePattern paired =
            forM_ (zip (tail $ inits paired) [1 :: Int ..]) $ \ (iterPaired, i) ->
                save debugMode (printf filenamePattern i) iterPaired
    save False "A.png" paired
    save False "B.png" =<< bakeTiles app paired
--     save True "debugA.png" paired
--     save True "debugB.png" =<< bakeTiles app paired
    saveIter True "debugA-%i.png" paired
    saveIter True "debugB-%i.png" =<< bakeTiles app paired


-- | load some random animated pixmaps (with padding pixels)
withSomePixmaps :: ([Animation Pixmap] -> IO a) -> IO a
withSomePixmaps cmd = do
    pngs <- fmap (pngDir </>) <$> getFiles pngDir (Just ".png")
    bracket (load pngs) free cmd
  where
    pngDir = (".." </> ".." </> "data" </> "png" </> "objects")

    load :: [FilePath] -> IO [Animation Pixmap]
    load =
        mapM (loadSymmetricPixmap (Position 1 1)) >=>
        return . groupBy (on (==) pixmapSize) >=>
        return . map mkAnim
    mkAnim :: Eq f => [f] -> Animation f
    mkAnim l = mkAnimation l [1]

    free :: [Animation Pixmap] -> IO ()
    free = fmapM_ (fmapM_ freePixmap)


-- | renders the given pixmaps to a new QPixmap.
renderToPixmap :: Bool -> Seconds -> [(Animation Pixmap, Position Double)]
    -> IO (Ptr QPixmap)
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
pixelEquality :: Ptr QPixmap -> Ptr QPixmap -> IO Bool
pixelEquality a b = do
    bracket
        ((,) <$> toImageQPixmap a <*> toImageQPixmap b)
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
--         putStrLn ("checking " ++ show pos)
        ap <- qRgbToColor =<< pixelQImage a pos
        bp <- qRgbToColor =<< pixelQImage b pos
        if ap /= bp
          then do
            putStrLn ("pixels not equal at " ++ show pos ++ ": " ++ pp (ap, bp))
            return False
          else check a b r
    check a n [] = return True
