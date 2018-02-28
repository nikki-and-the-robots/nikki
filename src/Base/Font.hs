{-# language ViewPatterns, ScopedTypeVariables #-}

module Base.Font (
    loadAlphaNumericFont,
    loadDigitFont,
    standardFont,
    digitFont,
    fontHeight,
    osdHeight,
    fontHeightOffset,

    Glyph(..),
    glyphSize,
    proseToGlyphs,
    wordWrap,
    -- for profiling
    searchGlyphs,
  ) where

import Prelude hiding (Word)

import Safe

import Data.Either
import Data.List as List
import Data.Abelian
import Data.Map (lookup, fromList)
import Data.Char
import qualified Data.Text as T

import Text.Parsec

import Control.Arrow (left)

import System.FilePath

import Graphics.Qt

import Utils hiding ((<|>))

import Base.Types
import Base.Constants
import Base.Paths
import Base.Pixmap
import Base.Prose
import Base.Font.ColorVariant


standardFontDir, digitFontDir :: FilePath
standardFontDir = pngDir </> "font"
digitFontDir = standardFontDir </> "digits"

alphaNumericFontColors, digitFontColors :: [Color]
alphaNumericFontColors = nub $
    standardFontColor :
    headerFontColor :
    white :
    yellow :
    []
digitFontColors = nub $
    standardFontColor :
    white :
    []

fontHeight :: Num n => n
fontHeight = 48

-- | height of the in game OSDs
osdHeight :: Double = fromUber 9

-- | vertical offset that has to be applied to have capital letters aligned.
fontHeightOffset :: Double
fontHeightOffset = fromUber (- 3.5)


-- * querying

standardFont, digitFont :: Application -> Font
standardFont = alphaNumericFont . applicationPixmaps
digitFont = pixmapsDigitFont . applicationPixmaps

proseToGlyphs :: Font -> Prose -> [Glyph]
proseToGlyphs font (Prose list) =
    concatMap inner list
  where
    inner :: (Color, T.Text) -> [Glyph]
    inner (color, string) =
        searchGlyphs (getColorVariant font color) string

-- | Returns the width of the given sequence of pixmaps,
-- including 1 üp for kerning
pixmapsWidth :: [Pixmap] -> Double
pixmapsWidth pixs =
    -- letters themselves
    sum (fmap (width . pixmapSize) pixs) +
    -- gaps in between
    max 0 (fromIntegral (Prelude.length pixs - 1)) * fromUber 1

-- | Returns the colorvariant for the given color.
getColorVariant :: Font -> Color -> ColorVariant
getColorVariant (Font m) color = case Data.Map.lookup color m of
    Just v -> v
    Nothing -> error ("font color variant missing: " ++ show color)

-- * word wrap

-- | a single word (as in "seperated by whitespaces")
data Word = Word {
    wordPixmaps :: [Glyph], -- word glyphs including following white space characters
    wordWidth :: Double, -- word width without counting white spaces, and counting
                         -- separating kerning pixels.
    wordYOffsetDelta :: Double -- word width including white spaces and
                               -- one kerning pixel for each character.
  }

glyphSize :: Glyph -> Size Double
glyphSize = pixmapSize . glyphPixmap

isSpaceGlyph :: Glyph -> Bool
isSpaceGlyph (Glyph c _) = T.all isSpace c
isSpaceGlyph ErrorGlyph{} = False

toWords :: [Glyph] -> [Word]
toWords [] = []
toWords glyphs =
    word : toWords rest
  where
    word = Word (nonSpaces ++ followingSpaces) wordWidth wordYOffsetDelta
    (nonSpaces, afterWord) = List.span (not . isSpaceGlyph) glyphs
    (followingSpaces, rest) = List.span isSpaceGlyph afterWord
    wordWidth = pixmapsWidth $ fmap glyphPixmap nonSpaces
    wordYOffsetDelta = pixmapsWidth (fmap glyphPixmap (nonSpaces ++ followingSpaces))
                       +~ fromUber 1

fromWords :: [Word] -> [Glyph]
fromWords = concatMap wordPixmaps

-- | implements a word wrap on a list of glyph pixmaps
wordWrapGlyphs :: [Double] -> [Glyph] -> [[Glyph]]
wordWrapGlyphs wrapWidths =
    toWords >>>
    inner wrapWidths 0 [] >>>
    fmap fromWords
  where
    inner :: [Double] -> Double -> [Word] -> [Word] -> [[Word]]
    inner [] w akk ws = inner wrapWidths w akk ws
    inner wrapWidths@(wrapWidth : restWidths) w akk (a : r) =
        if List.null akk || (w + wordWidth a <= wrapWidth) then
            inner wrapWidths (w + wordYOffsetDelta a + fromUber 1) (a : akk) r
          else
            List.reverse akk : inner restWidths 0 [] (a : r)
    inner _ _ akk [] = List.reverse akk : []

-- | Wraps a Prose according to the given list of line widths.
-- Cycles the list of line widths.
-- If you have a constant line width, use @wordWrap font [lineWidth] prose@.
wordWrap :: Font -> [Double] -> Prose -> [[Glyph]]
wordWrap font ws = wordWrapGlyphs ws . proseToGlyphs font


-- * loading

loadFont fontColors dir = do
    letterFiles <- getDataFiles dir (Just "png")
    io $ toFont fontColors =<< mapM loadLetter letterFiles

-- | loads the standard variable-width font
loadAlphaNumericFont :: IO Font
loadAlphaNumericFont = loadFont alphaNumericFontColors standardFontDir

-- | loads the digits-only font
loadDigitFont :: IO Font = loadFont digitFontColors digitFontDir

-- | Converts loaded pixmaps to a font.
-- Also sorts the letter pixmaps (longest keys first).
-- Does only load the standard color variant at the moment.
toFont :: [Color] -> [(Either T.Text ErrorSymbol, Pixmap)] -> IO Font
toFont fontColors m =
    toColorVariants fontColors $ mkColorVariant letters errorSymbol
  where
    letters :: [(T.Text, Pixmap)]
    letters = fmap (\ k -> (k, lookupJust (Left k) m)) $ lefts $ fmap fst m
    errorSymbol = lookupJustNote "error symbol not found" (Right ErrorSymbol) m

-- | converts the loaded color variant (white/black) to
-- the standardColorVariants
-- and returns the Font.
-- Also frees the loaded colorVariant.
toColorVariants :: [Color] -> ColorVariant -> IO Font
toColorVariants fontColors loadedVariant = do
    qBlack <- colorToQRgb black
    qWhite <- colorToQRgb white
    qTransparent <- colorToQRgb transparent
    let colorMapping :: QRgb -> QRgb -> QRgb
        colorMapping foreground c =
            if c == qBlack then qTransparent else
            if c == qWhite then foreground else
            error ("font pixmaps should consist of black and white only. (" ++
                    show c ++ ")")

    colorVariants <- forM fontColors $ \ foreground -> do
        qForeground <- colorToQRgb foreground
        variant <- newColorVariant (colorMapping qForeground) loadedVariant
        return (foreground, variant)
    return $ Font $ fromList colorVariants

data ErrorSymbol = ErrorSymbol
  deriving (Eq, Ord)

loadLetter :: FilePath -> IO (Either T.Text ErrorSymbol, Pixmap)
loadLetter file = do
    pixmap <- loadSymmetricPixmap (Position 1 1) file
    return (textString, pixmap)
  where
    textString :: Either T.Text ErrorSymbol
    textString = left (T.pack . fmap chr) $ parseFileName (takeBaseName file)

parseFileName :: String -> Either [Int] ErrorSymbol
parseFileName name = do
    case parse parser ("letterName: " ++ name) name of
        Left err -> error $ show err
        Right x -> x
  where
    parser :: Parsec String () (Either [Int] ErrorSymbol)
    parser = none <|> numbers

    none = string "NONE" >> return (Right ErrorSymbol)

    numbers = do
        numberStrings :: [String] <- sepBy1 (many1 digit) (char '_')
        return $ Left $ fmap read numberStrings
