{-# language MultiParamTypeClasses, DeriveDataTypeable, ViewPatterns, ScopedTypeVariables #-}

-- | backgrounds are rendered statically centered on the screen,
-- no matter which layer they are in. Every background consists of multiple
-- pixmaps in different sizes. When rendered (in editor mode
-- and in game mode) they will use the smallest pixmap that will
-- cover the whole screen area, if one exists. If not, it will
-- use the pixmap that has the greatest width.
-- There are some values that are relevant for other objects,
-- but not for backgrounds: size and position.
-- Size will be the size of the smallest pixmap (width-wise)
-- (mainly cause it needs to be something)
-- and position should always be (EditorPosition 0 0)
-- (so you know where it is, if you want to delete it).


module Sorts.Background (sorts) where


import Data.Typeable
import Data.Abelian
import Data.List
import Data.Maybe

import Control.Applicative

import System.FilePath

import Graphics.Qt

import Utils

import Base hiding (backgrounds)

import Object


backgrounds =
    "blue" :
    []


sorts :: RM [Sort_]
sorts = map Sort_ <$> mapM mkSort backgrounds

mkSort :: String -> RM BSort
mkSort name = do
                                -- zeropadding :)
    pixmaps <- mapM (loadPixmap zero) =<< getPngPaths name
    return $ BSort name (sort pixmaps)
  where
    sort = sortBy (withView (width . pixmapSize) compare)

getPngPaths :: String -> RM [FilePath]
getPngPaths n =
    getDataFiles (pngDir </> "backgrounds" </> n) (Just ".png")

data BSort = BSort {
    name :: String,
    pixmaps :: [Pixmap]
  }
    deriving (Show, Typeable)

instance Sort BSort () where
    sortId s = SortId $ ("background/" ++ name s)
    size s = pixmapSize $ head $ pixmaps s -- does not make much sense
                                           -- (except for the cursor and iconified rendering)
    renderIconified sort ptr =
        renderPixmapSimple ptr (head $ pixmaps sort)
    renderEditorObject ptr offset eo = do
        resetMatrix ptr
        windowSize <- fmap fromIntegral <$> sizeQPainter ptr
        renderWholeScreenPixmap ptr windowSize (pixmaps $ editorSort eo)
    initialize sort mSpace ep Nothing = return ()
    immutableCopy = return
    chipmunks = const []
    renderObject _ s ptr offset _ = do
        windowSize <- fmap fromIntegral <$> sizeQPainter ptr
        let mPix = pickWholeScreenPixmap windowSize $ pixmaps s
            pix = fromMaybe (last $ pixmaps s) mPix
            pos = sizeToPosition $ fmap (fromIntegral . round . (/ 2)) (windowSize -~ pixmapSize pix)
        return [RenderPixmap pix (pos -~ offset) Nothing]
