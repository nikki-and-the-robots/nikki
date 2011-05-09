{-# language ScopedTypeVariables, ViewPatterns #-}

module Base.Font (
    loadAlphaNumericFont,
    freeFont,
    fontHeight,

    Glyph(..),
    glyphSize,
    proseToGlyphs,
    wordWrap,
  ) where


import Safe

import Data.Either
import Data.List as List
import Data.Abelian
import Data.Map ((!), lookup, fromList)
import Data.Text as Text hiding (concatMap)
import Data.Char

import Text.Parsec

import Control.Arrow (left)

import System.FilePath

import Graphics.Qt

import Utils

import Base.Types
import Base.Constants
import Base.Paths
import Base.Pixmap
import Base.Prose
import Base.Font.ColorVariant


standardFontDir :: FilePath
standardFontDir = "png" </> "font"

fontColors :: [Color]
fontColors = nub $
    standardFontColor :
    headerFontColor :
    white :
    []

fontHeight :: Num n => n
fontHeight = 36

-- * querying

standardFont :: Application -> Font
standardFont = alphaNumericFont . applicationPixmaps

proseToGlyphs :: Application -> Prose -> [Glyph]
proseToGlyphs app (Prose list) =
    concatMap inner list
  where
    inner :: (Color, Text) -> [Glyph]
    inner (color, string) =
        searchGlyphs (getColorVariant (standardFont app) color) string

searchGlyphs :: ColorVariant -> Text -> [Glyph]
searchGlyphs variant =
    inner (glyphs variant)
  where
    inner _ string | Text.null string = []
    inner ((key, pixmap) : r) string =
        if key `Text.isPrefixOf` string then
            Glyph key pixmap : searchGlyphs variant (Text.drop (Text.length key) string)
        else
            inner r string
    inner [] string = ErrorGlyph (errorSymbol variant) : searchGlyphs variant (Text.tail string)

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

-- | a letter with its graphical representation
data Glyph
    = Glyph {
        character :: Text,
        glyphPixmap :: Pixmap
      }
    | ErrorGlyph {glyphPixmap :: Pixmap}
  deriving (Show)

glyphSize :: Glyph -> Size Double
glyphSize = pixmapSize . glyphPixmap

isSpaceGlyph :: Glyph -> Bool
isSpaceGlyph (Glyph c _) = Text.all isSpace c

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
wordWrapGlyphs :: Double -> [Glyph] -> [[Glyph]]
wordWrapGlyphs wrapWidth =
    toWords >>>
    inner 0 [] >>>
    fmap fromWords
  where
    inner :: Double -> [Word] -> [Word] -> [[Word]]
    inner w akk (a : r) =
        if List.null akk || (w + wordWidth a <= wrapWidth) then
            inner (w + wordYOffsetDelta a + fromUber 1) (a : akk) r
          else
            List.reverse akk : inner 0 [] (a : r)
    inner _ akk [] = List.reverse akk : []

wordWrap :: Application -> Double -> Prose -> [[Glyph]]
wordWrap app w = wordWrapGlyphs w . proseToGlyphs app


-- * loading

-- | loads the standard variable-width font
loadAlphaNumericFont :: RM Font
loadAlphaNumericFont = do
    letterFiles <- getDataFiles standardFontDir (Just "png")
    io $ toFont =<< mapM loadLetter letterFiles

-- | Converts loaded pixmaps to a font.
-- Also sorts the letter pixmaps (longest keys first).
-- Does only load the standard color variant at the moment.
toFont :: [(Either Text ErrorSymbol, Pixmap)] -> IO Font
toFont m =
    toColorVariants $ ColorVariant sortedLetters errorSymbol
  where
    letters = fmap (\ k -> (k, lookupJust (Left k) m)) $ lefts $ fmap fst m
    sortedLetters = List.reverse $ sortBy shortestKeyFirst letters
    errorSymbol = lookupJustNote "error symbol not found" (Right ErrorSymbol) m

    shortestKeyFirst :: (Text, b) -> (Text, b) -> Ordering
    shortestKeyFirst = withView (Text.length . fst) compare

-- | converts the loaded color variant (white/black) to
-- the standardColorVariants
-- and returns the Font.
-- Also frees the loaded colorVariant.
toColorVariants :: ColorVariant -> IO Font
toColorVariants loadedVariant = do
    qBlack <- colorToQRgb black
    qWhite <- colorToQRgb white
    qTransparent <- colorToQRgb transparent
    let colorMapping :: QRgb -> QRgb -> QRgb
        colorMapping foreground c =
            if c == qBlack then qTransparent else
            if c == qWhite then foreground else
            error "font pixmaps should consist of black and white only."

    colorVariants <- forM fontColors $ \ foreground -> do
        qForeground <- colorToQRgb foreground
        variant <- newColorVariant (colorMapping qForeground) loadedVariant
        return (foreground, variant)
    freeColorVariant loadedVariant
    return $ Font $ fromList colorVariants

data ErrorSymbol = ErrorSymbol
  deriving (Eq, Ord)

loadLetter :: FilePath -> IO (Either Text ErrorSymbol, Pixmap)
loadLetter file = do
    pixmap <- loadSymmetricPixmap (Position 1 1) file
    return (textString, pixmap)
  where
    textString = left (pack . fmap chr) $ parseFileName (takeBaseName file)

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


-- * unloading

-- | frees the memory taken by a font
freeFont :: Font -> IO ()
freeFont (Font variants) = forM_ variants freeColorVariant
