{-# language MultiParamTypeClasses, DeriveDataTypeable #-}

module Sorts.Grids where


import Data.Generics
import Data.Abelian

import Control.Monad

import System.FilePath

import Graphics.Qt

import Paths
import Utils

import Base.Constants
import Base.Pixmap

import Object


grids :: [FilePath]
grids = [
    "multilayers/grid-white"
  ]


sorts :: IO [Sort_]
sorts = mapM mkSort grids


mkSort :: String -> IO Sort_
mkSort name = do
    path <- getDataFileName (pngDir </> name <.> "png")
    pixmap <- loadPixmap 1 path
    return $ Sort_ $ GSort name pixmap

data GSort = GSort {
    name :: String,
    gridPixmap :: Pixmap
  }
    deriving (Show, Typeable)

data Grid = Grid {
    position :: (Position Double)
  }
    deriving (Show, Typeable)

instance Sort GSort Grid where
    sortId s = SortId ("grid/" ++ name s)
    size s = pixmapSize $ gridPixmap s

    -- if rendered without scaling, the grid will be multiplied
    sortRender sort ptr offset ep Nothing = do
        let pos = editorPosition2QtPosition sort ep
        renderGrid ptr offset sort pos
    -- if rendered with scaling, the grid image is rendered once.
    sortRender sort ptr offset ep (Just scaling) =
        sortRenderSinglePixmap (gridPixmap sort) sort ptr offset ep (Just scaling)

    initialize sort mSpace ep Nothing = do
        let pos = editorPosition2QtPosition sort ep
        return $ Grid pos

    chipmunk = error "chipmunk in Sorts.Grids"

    render o s ptr offset now =
        renderGrid ptr offset s (position o)


renderGrid :: Ptr QPainter -> Offset Double -> GSort -> Position Double -> IO ()
renderGrid ptr offset sort pos = do
    let pix = gridPixmap sort

    resetMatrix ptr

    translate ptr (fmap fromIntegral (pixmapOffset pix))

    windowSize <- fmap fromIntegral <$> sizeQPainter ptr
    let positions = calculateGridRenderPositions windowSize (size sort) (pos +~ offset)

    forM_ positions $ \ p -> do
        translate ptr p
        drawPixmap ptr zero (pixmap pix)
        translate ptr (negateAbelian p)

-- | calculates where to render the grid, so that every space of the screen is filled
calculateGridRenderPositions :: Size Double -> Size Double
    -> Position Double -- ^ actual render position (including offset)
    -> [Position Double]
calculateGridRenderPositions windowSize tileSize position =
    map toRenderPosition intPositions
  where
    toRenderPosition :: (Int, Int) -> Position Double
    toRenderPosition (x, y) = position +~ (Position (fromIntegral x * w) (fromIntegral y * h))
    Size w h = tileSize
    intPositions :: [(Int, Int)]
    intPositions = cartesian [xmin .. xmax] [ymin .. ymax]

    xmin = floor ((0 - positionX position) / width tileSize)
    xmax = floor ((width windowSize - positionX position) / width tileSize)
    ymin = floor ((0 - positionY position) / height tileSize)
    ymax = floor ((height windowSize - positionY position) / height tileSize)












