{-# language ScopedTypeVariables #-}

module Base.Font (
    loadAlphaNumericFont,
    freeFont,
    fontHeight,
    renderLine,
  ) where


import Safe

import qualified Data.ByteString as BS
import Data.Either
import Data.List
import Data.Abelian

import Text.Parsec

import Codec.Binary.UTF8.Light

import Control.Arrow (left)

import System.FilePath

import Graphics.Qt

import Utils hiding ((<|>))

import Base.Types
import Base.Constants
import Base.Paths
import Base.Pixmap
import Base.Prose


standardFontDir :: FilePath
standardFontDir = "png" </> "font"

-- * querying

-- | returns the standard height of the font
fontHeight :: Font -> Double
fontHeight = height . pixmapSize . errorSymbol

-- | returns a list of pixmaps that represent the given Prose text.
selectLetterPixmaps :: Font -> Prose -> [Pixmap]
selectLetterPixmaps font prose =
    inner (letters font) (getByteString prose)
  where
    inner :: [(BS.ByteString, Pixmap)] -> BS.ByteString -> [Pixmap]
    inner _ string | BS.null string = []
    inner ((key, pixmap) : r) string =
        if key `BS.isPrefixOf` string then
            pixmap : inner (letters font) (BS.drop (BS.length key) string)
        else
            inner r string
    inner [] string = errorSymbol font : inner (letters font) (BS.tail string)


-- * loading

-- | loads the standard variable-width font
loadAlphaNumericFont :: RM Font
loadAlphaNumericFont = do
    letterFiles <- getDataFiles standardFontDir (Just "png")
    io $ toFont <$> mapM loadLetter letterFiles

-- | Converts loaded pixmaps to a font.
-- Also sorts the letter pixmaps (longest keys first).
toFont :: [(Either BS.ByteString ErrorSymbol, Pixmap)] -> Font
toFont m =
    Font sortedLetters errorSymbol
  where
    letters = map (\ k -> (k, lookupJust (Left k) m)) $ lefts $ map fst m
    sortedLetters = reverse $ sortBy shortestKeyFirst letters
    errorSymbol = lookupJustNote "error symbol not found" (Right ErrorSymbol) m

    shortestKeyFirst :: (BS.ByteString, b) -> (BS.ByteString, b) -> Ordering
    shortestKeyFirst = withView (BS.length . fst) compare

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
        return $ Left $ map read numberStrings


-- * unloading

-- | frees the memory taken by a font
freeFont :: Font -> IO ()
freeFont = error "freeFont"


-- * rendering

-- | Returns a rendering action to render a line of text
-- and the size of the renderings.
-- Does not alter the painter matrix.
renderLine :: Font -> Prose -> (Ptr QPainter -> IO (), Size Double)
renderLine font text =
    (\ ptr -> action ptr 0 textPixmaps, size)
  where
    size =
        Size textWidth textHeight
    textWidth =
        -- letters themselves
        sum (map (width . pixmapSize) textPixmaps) +
        -- gaps in between
        max 0 (fromIntegral (Prelude.length textPixmaps - 1)) * fromUber 1
    textHeight = fontHeight font

    action :: Ptr QPainter -> Int -> [Pixmap] -> IO ()
    action ptr widthOffset (pix : r) = do
        drawPixmap ptr
            (Position widthOffset 0 +~ fmap round (pixmapOffset pix))
            (pixmap pix)
        action ptr (widthOffset + round (width (pixmapSize pix)) + fromUber 1) r
    action _ _ [] = return ()
    -- sequence of pixmaps to be rendered
    textPixmaps :: [Pixmap]
    textPixmaps = selectLetterPixmaps font text
