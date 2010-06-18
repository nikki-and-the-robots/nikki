
module Base.Pixmap where


import Data.Abelian

import Graphics.Qt

import Utils

import Base.Constants


data Pixmap = Pixmap {
    pixmap :: Ptr QPixmap,
    pixmapSize :: Size Double,
    pixmapOffset :: Position Int
  }
    deriving Show

loadPixmap :: Int -> FilePath -> IO Pixmap
loadPixmap padding path = do
    pix <- newQPixmap path
    size <- sizeQPixmap pix
    return $ Pixmap
        pix
        (fmap (fromIntegral . subtract (2 * padding)) size)
        (Position (- padding) (- padding))


renderPixmap :: Ptr QPainter -> Offset Double
    -> Position Double -> Maybe Double
    -> Pixmap -> IO ()
renderPixmap ptr offset position mAngle pix = do
    resetMatrix ptr
    translate ptr offset

    translate ptr position 
    whenMaybe mAngle $ \ angle ->
        rotate ptr (rad2deg angle)
    translate ptr (fmap fromIntegral (pixmapOffset pix))

    drawPixmap ptr zero (pixmap pix)


--     Qt.resetMatrix painter
--     translate painter worldOffset
-- 
--     (position, rad) <- getRenderPosition chipmunk
-- 
--     translate painter position
--     Qt.rotate painter (rad2deg rad)
-- 
--     Qt.drawPixmap painter zero p


--     resetMatrix ptr
--     translate ptr offset
--     let (Size width height) = size sort
--         (factor, innerOffset) = case scaling of
--             Just x ->
--                 squeezeScaling x $ size sort
--             Nothing -> (1, zero)
-- 
--         p = Position (x - 1) (y - 1 - height * factor) +~ innerOffset
-- 
--     translate ptr p
--     Qt.scale ptr factor factor
-- 
--     drawPixmap ptr zero pixmap

