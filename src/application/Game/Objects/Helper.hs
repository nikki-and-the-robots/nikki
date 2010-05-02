
module Game.Objects.Helper where


import Utils

import Data.Abelian

import Graphics.Qt as Qt

import Physics.Chipmunk as CM


-- * chipmunk initialisation

mkStandardPolys :: Size Double -> ([ShapeType], Vector)
mkStandardPolys (Size w h) =
     ([rect], baryCenterOffset)
  where
    rect =
        Polygon [
            Vector (- wh) (- hh),
            Vector (- wh) hh,
            Vector wh hh,
            Vector wh (- hh)
          ]
    wh = w / 2
    hh = h / 2
    baryCenterOffset = Vector wh hh


-- * rendering

renderChipmunk :: Ptr QPainter -> Qt.Position Double -> Ptr QPixmap -> Chipmunk -> IO ()
renderChipmunk painter worldOffset p chipmunk = do
    resetMatrix painter
    translate painter worldOffset

    (position, rad) <- CM.getRenderPosition chipmunk

    translateVector painter position
    Qt.rotate painter (rad2deg rad)

    drawPixmap painter zero p

translateSpriteToCenter :: Ptr QPainter -> Size Int -> IO ()
translateSpriteToCenter ptr (Size width height) = do
    let wh = fromIntegral width / 2
        hh = fromIntegral height / 2
    translate ptr (Position (- wh) (- hh))



