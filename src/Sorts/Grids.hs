{-# language MultiParamTypeClasses, DeriveDataTypeable #-}

module Sorts.Grids where


import Data.Generics
import Data.Abelian

import Control.Monad

import System.FilePath

import Graphics.Qt

import Utils

import Base

import Object


grids :: [FilePath]
grids = [
    "multilayers/grid-white"
  ]


sorts :: M [Sort_]
sorts = mapM mkSort grids


mkSort :: String -> M Sort_
mkSort name = do
    path <- getDataFileName (pngDir </> name <.> "png")
    pixmap <- loadPixmap (Position 1 1) path
    return $ Sort_ $ GSort name pixmap

data GSort = GSort {
    name :: String,
    gridPixmap :: Pixmap
  }
    deriving (Show, Typeable)

data Grid = Grid {
    gridSize :: Size Double,
    position :: (Position Double)
  }
    deriving (Show, Typeable)

instance Sort GSort Grid where
    sortId s = SortId ("grid/" ++ name s)
    size s = pixmapSize $ gridPixmap s

    -- if rendered without scaling, the grid will be multiplied
    sortRender sort ptr Iconified = 
        renderPixmapSimple ptr (gridPixmap sort)
    sortRender sort ptr (InScene offset) = do
        -- render the grid
        windowSize <- fmap fromIntegral <$> sizeQPainter ptr
        renderGrid ptr sort offset windowSize

    initialize sort mSpace ep Nothing = do
        let pos = editorPosition2QtPosition sort ep
        return $ Grid (size sort) pos

    immutableCopy = return

    chipmunks = const []

    render o s ptr offset now = do
        resetMatrix ptr
        let offsetPlusPosition = offset +~ position o
        translate ptr offsetPlusPosition
        windowSize <- fmap fromIntegral <$> sizeQPainter ptr
        renderGrid ptr s offsetPlusPosition windowSize


renderGrid :: Ptr QPainter -> GSort -> Position Double -> Size Double -> IO ()
renderGrid ptr sort position windowSize = do
    let pix = gridPixmap sort
        positions = calculateGridRenderPositions windowSize (size sort) position

    forM_ positions $ \ p -> do
        translate ptr p
        renderPixmapSimple ptr pix
        translate ptr (negateAbelian p)

-- | calculates where to render the grid, so that every space of the screen is filled
calculateGridRenderPositions :: Size Double -> Size Double
    -> Position Double -- ^ actual render position
    -> [Position Double]
calculateGridRenderPositions windowSize tileSize position =
    map toRenderPosition intPositions
  where
    toRenderPosition :: (Int, Int) -> Position Double
    toRenderPosition (x, y) = 
        Position (fromIntegral x * width tileSize) (fromIntegral y * height tileSize)
    intPositions :: [(Int, Int)]
    intPositions = cartesian [xmin .. xmax] [ymin .. ymax]

    xmin = floor ((0 - positionX position) / width tileSize)
    xmax = floor ((width windowSize - positionX position) / width tileSize)
    ymin = floor ((0 - positionY position) / height tileSize)
    ymax = floor ((height windowSize - positionY position) / height tileSize)

