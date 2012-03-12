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
    withQApplication $ \ qApp -> do
    pixmaps <- loadApplicationPixmaps
    let variant = colorVariants (alphaNumericFont pixmaps) ! standardFontColor
    io $ defaultMain $
        bench "searchGlyphsOld" (b (searchGlyphs variant)) :
        bench "searchGlyphs"    (b (searchGlyphs variant)) :
        []

b :: (T.Text -> [Glyph]) -> Pure
b f = nf (fmap f) (replicate 1 "This is an example text")

instance NFData Glyph where
    rnf (Glyph a b) = rnf a `seq` rnf b
    rnf (ErrorGlyph a) = rnf a
