{-# language ScopedTypeVariables #-}

module Base.Font (
    loadAlphaNumericFont,
    freeFont,
    fontHeight,
    renderLine,
    renderLineSimple,
  ) where


import Safe

import qualified Data.ByteString as BS
import Data.Either
import Data.List
import Data.Abelian
import Data.Map

import Text.Parsec

import Codec.Binary.UTF8.Light

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

standardFontColor :: Color
standardFontColor = white

-- * querying

-- | returns the standard height of the font
fontHeight :: Font -> Double
fontHeight = height . pixmapSize . errorSymbol . (! standardFontColor) . colorVariants

-- | returns a list of pixmaps that represent the given Prose text.
selectLetterPixmaps :: ColorVariant -> Prose -> [Pixmap]
selectLetterPixmaps variant prose =
    inner (glyphs variant) (getByteString prose)
  where
    inner :: [(BS.ByteString, Pixmap)] -> BS.ByteString -> [Pixmap]
    inner _ string | BS.null string = []
    inner ((key, pixmap) : r) string =
        if key `BS.isPrefixOf` string then
            pixmap : inner (glyphs variant) (BS.drop (BS.length key) string)
        else
            inner r string
    inner [] string = errorSymbol variant : inner (glyphs variant) (BS.tail string)


-- * loading

-- | loads the standard variable-width font
loadAlphaNumericFont :: RM Font
loadAlphaNumericFont = do
    letterFiles <- getDataFiles standardFontDir (Just "png")
    io $ toFont =<< mapM loadLetter letterFiles

-- | Converts loaded pixmaps to a font.
-- Also sorts the letter pixmaps (longest keys first).
-- Does only load the standard color variant at the moment.
toFont :: [(Either BS.ByteString ErrorSymbol, Pixmap)] -> IO Font
toFont m =
    toStandardColorVariant $ ColorVariant sortedLetters errorSymbol
  where
    letters = fmap (\ k -> (k, lookupJust (Left k) m)) $ lefts $ fmap fst m
    sortedLetters = reverse $ sortBy shortestKeyFirst letters
    errorSymbol = lookupJustNote "error symbol not found" (Right ErrorSymbol) m

    shortestKeyFirst :: (BS.ByteString, b) -> (BS.ByteString, b) -> Ordering
    shortestKeyFirst = withView (BS.length . fst) compare

-- | converts the loaded color variant (white/black) to
-- the standardColorVariant (standardFontColor/transparent)
-- and returns the initial Font.
-- Also frees the loaded colorVariant.
toStandardColorVariant :: ColorVariant -> IO Font
toStandardColorVariant loadedVariant = do
    standardColorVariant <- newColorVariant colorMapping loadedVariant
    freeColorVariant loadedVariant
    return $ Font $ fromList [(standardFontColor, standardColorVariant)]
  where
    colorMapping :: Color -> Color
    colorMapping c =
        if c == black then transparent else
        if c == white then standardFontColor else
        error "font pixmaps should consist of black and white only."

data ErrorSymbol = ErrorSymbol
  deriving (Eq, Ord)

loadLetter :: FilePath -> IO (Either BS.ByteString ErrorSymbol, Pixmap)
loadLetter file = do
    pixmap <- loadPixmap (Position 1 1) file
    return (utf8String, pixmap)
  where
    utf8String = left encodeUTF8 $ parseFileName (takeBaseName file)

parseFileName :: String -> Either [Word32] ErrorSymbol
parseFileName name = do
    case parse parser ("letterName: " ++ name) name of
        Left err -> error $ show err
        Right x -> x
  where
    parser :: Parsec String () (Either [Word32] ErrorSymbol)
    parser = none <|> numbers

    none = string "NONE" >> return (Right ErrorSymbol)

    numbers = do
        numberStrings :: [String] <- sepBy1 (many1 digit) (char '_')
        return $ Left $ fmap read numberStrings


-- * unloading

-- | frees the memory taken by a font
freeFont :: Font -> IO ()
freeFont (Font variants) = forM_ variants freeColorVariant


-- * rendering

-- | Like renderLine, but renders with the current GL matrix.
-- Does not alter the GL matrix.
renderLineSimple :: Font -> Color -> Prose -> Ptr QPainter -> IO ()
renderLineSimple font color text ptr = do
    (action, _) <- renderLine font color text
    action ptr

-- | Returns a rendering action to render a line of text
-- and the size of the renderings.
-- Does not alter the painter matrix.
renderLine :: Font -> Color -> Prose -> IO (Ptr QPainter -> IO (), Size Double)
renderLine font color text = do
    variant <- getColorVariant font color
    let pixs = textPixmaps variant
    return (\ ptr -> action ptr 0 pixs, size pixs)
  where
    size pixs =
        Size (textWidth pixs) textHeight
    textWidth pixs =
        -- letters themselves
        sum (fmap (width . pixmapSize) pixs) +
        -- gaps in between
        max 0 (fromIntegral (Prelude.length pixs - 1)) * fromUber 1
    textHeight = fontHeight font

    action :: Ptr QPainter -> Int -> [Pixmap] -> IO ()
    action ptr widthOffset (pix : r) = do
        drawPixmap ptr
            (Position widthOffset 0 +~ fmap round (pixmapOffset pix))
            (pixmap pix)
        action ptr (widthOffset + round (width (pixmapSize pix)) + fromUber 1) r
    action _ _ [] = return ()
    -- sequence of pixmaps to be rendered
    textPixmaps :: ColorVariant -> [Pixmap]
    textPixmaps variant = selectLetterPixmaps variant text


-- | Returns the colorvariant for the given color.
getColorVariant :: Font -> Color -> IO ColorVariant
getColorVariant (Font m) color = case Data.Map.lookup color m of
    Just v -> return v
    Nothing -> error ("font color variant missing: " ++ show color)
