{-# language OverloadedStrings #-}

-- | benchmarking Base.Font.searchGlyphs


import qualified Data.Text as T
import Data.Initial
import Data.Map

import Control.Monad.Reader
import Control.DeepSeq

import Graphics.Qt

import Utils

import Base
import Base.Font

import Criterion.Main


main =
    flip runReaderT (savedConfigurationToConfiguration initial) $
    withQApplication $ \ qApp ->
    withApplicationPixmaps $ \ pixmaps -> io $ do
        let variant = colorVariants (alphaNumericFont pixmaps) ! standardFontColor
        defaultMain (
            bench "searchGlyphs" (b (searchGlyphs variant)) :
            bench "searchGlyphsOld" (b (searchGlyphs variant)) :
            [])

b :: (T.Text -> [Glyph]) -> Pure
b f = nf f "This is an example text"

instance NFData Glyph where
    rnf (Glyph a b) = rnf a `seq` rnf b
    rnf (ErrorGlyph a) = rnf a
