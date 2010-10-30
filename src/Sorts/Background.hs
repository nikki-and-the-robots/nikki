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


import Safe

import Data.Typeable
import Data.Abelian
import Data.List
import Data.Maybe

import Control.Applicative
import Control.Monad

import System.FilePath
import System.Directory

import Graphics.Qt

import Utils
import Paths
import Base.Pixmap
import Base.Constants

import Object


backgrounds = [
    "blue"
  ]


sorts :: IO [Sort_]
sorts = map Sort_ <$> mapM mkSort backgrounds

mkSort :: String -> IO BSort
mkSort name = do
                                -- zeropadding :)
    pixmaps <- mapM (loadPixmap 0) =<< getPngPaths name
    return $ BSort name (sort pixmaps)
  where
    sort = sortBy (withView (width . pixmapSize) compare)

getPngPaths :: String -> IO [FilePath]
getPngPaths n =
    getDataFiles ".png" (pngDir </> "backgrounds" </> n)

data BSort = BSort {
    name :: String,
    pixmaps :: [Pixmap] -- 
  }
    deriving (Show, Typeable)

instance Sort BSort () where
    sortId s = SortId $ ("background/" ++ name s)
    size s = pixmapSize $ head $ pixmaps s -- does not make much sense
                                           -- (except for the cursor and iconified rendering)
    sortRender sort ptr Iconified =
        renderPixmapSimple ptr (head $ pixmaps sort)
    sortRender sort ptr (InScene offset) =
        render () sort ptr offset 0
    initialize sort mSpace ep Nothing = return ()
    immutableCopy = return
    chipmunks = const []
    render _ s ptr _ _ = do
        resetMatrix ptr
        windowSize <- fmap fromIntegral <$> sizeQPainter ptr
        let mPix = pickPixmap windowSize $ pixmaps s
            pix = fromMaybe (last $ pixmaps s) mPix
            offset = sizeToPosition $ fmap (round . (/ 2)) (windowSize -~ pixmapSize pix)
        when (isNothing mPix) $
            -- background image is too small
            clearScreen ptr
        drawPixmap ptr offset (pixmap pix)

-- | returns the smallest pixmap that can cover the whole window, if it exists
pickPixmap :: Size Double -> [Pixmap] -> Maybe Pixmap
pickPixmap windowSize list =
    headMay $ filter isBiggerThanWindow list
  where
    isBiggerThanWindow (pixmapSize -> pixmapSize) =
        withView width (>) pixmapSize windowSize &&
        withView height (>) pixmapSize windowSize
