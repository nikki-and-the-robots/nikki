{-# language OverloadedStrings #-}

module Base.Font.ColorVariant (
    newColorVariant,
    mkColorVariant,
    searchGlyphs,
  ) where


import qualified Data.Text as T
import Data.Map (fromList, lookup)

import Graphics.Qt

import Utils

import Base.Types
import Base.Pixmap


-- | creates a new colorvariant as a copy of the given
-- colorvariant while mapping the colors using the given function
newColorVariant :: (QRgb -> QRgb) -> ColorVariant -> IO ColorVariant
newColorVariant colorMapping (ColorVariant longest glyphs errorSymbol) = do
    newGlyphs <- fmapM (copyAndMapColors colorMapping) glyphs
    newErrorSymbol <- copyAndMapColors colorMapping errorSymbol
    return $ ColorVariant longest newGlyphs newErrorSymbol

copyAndMapColors :: (QRgb -> QRgb) -> Pixmap -> IO Pixmap
copyAndMapColors colorMapping pix = do
    new <- copyPixmap pix
    mapColors colorMapping new

mkColorVariant :: [(T.Text, Pixmap)] -> Pixmap -> ColorVariant
mkColorVariant letters errorSymbol =
    ColorVariant longest letterMap errorSymbol
  where
    longest = maximum $ map (T.length . fst) letters
    letterMap = fromList letters

searchGlyphs :: ColorVariant -> T.Text -> [Glyph]
searchGlyphs variant t =
    inner (longest variant) t
  where
    inner :: Int -> T.Text -> [Glyph]
    inner _ "" = []
    inner 0 string =
        ErrorGlyph (errorSymbol variant) : inner (longest variant) (T.tail string)
    inner n string =
        maybe
            (inner (pred n) string) -- search shorter matches
            (\ pixmap -> Glyph prefix pixmap : inner (longest variant) rest) -- found
            (Data.Map.lookup prefix (glyphs variant))
      where
        (prefix, rest) = T.splitAt n string
