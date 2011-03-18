
module Base.Font.ColorVariant (
    newColorVariant,
    freeColorVariant,
  ) where


import Graphics.Qt

import Utils

import Base.Types
import Base.Pixmap


-- | creates a new colorvariant as a copy of the given
-- colorvariant while mapping the colors using the given function
newColorVariant :: (QRgb -> QRgb) -> ColorVariant -> IO ColorVariant
newColorVariant colorMapping (ColorVariant glyphs errorSymbol) = do
    newGlyphs <- fmapM (\ (a, b) -> tuple a <$> copyAndColorPix colorMapping b) glyphs
    newErrorSymbol <- copyAndColorPix colorMapping errorSymbol
    return $ ColorVariant newGlyphs newErrorSymbol

copyAndColorPix :: (QRgb -> QRgb) -> Pixmap -> IO Pixmap
copyAndColorPix colorMapping pix = do
    new <- copyPixmap pix
    mapPixels colorMapping new

freeColorVariant :: ColorVariant -> IO ()
freeColorVariant (ColorVariant glyphs errorSymbol) = do
    mapM_ (freePixmap . snd) glyphs
    freePixmap errorSymbol
