
module Base.Font.ColorVariant (
    newColorVariant,
  ) where


import Graphics.Qt

import Utils

import Base.Types
import Base.Pixmap


-- | creates a new colorvariant as a copy of the given
-- colorvariant while mapping the colors using the given function
newColorVariant :: (QRgb -> QRgb) -> ColorVariant -> IO ColorVariant
newColorVariant colorMapping (ColorVariant glyphs errorSymbol) = do
    newGlyphs <- fmapM (\ (a, b) -> tuple a <$> copyAndMapColors colorMapping b) glyphs
    newErrorSymbol <- copyAndMapColors colorMapping errorSymbol
    return $ ColorVariant newGlyphs newErrorSymbol

copyAndMapColors :: (QRgb -> QRgb) -> Pixmap -> IO Pixmap
copyAndMapColors colorMapping pix = do
    new <- copyPixmap pix
    mapColors colorMapping new
