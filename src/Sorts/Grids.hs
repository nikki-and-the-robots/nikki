{-# language MultiParamTypeClasses, DeriveDataTypeable #-}

module Sorts.Grids where


import Data.Generics
import Data.Abelian

import System.FilePath

import Graphics.Qt

import Utils

import Base

import Object


grids :: [FilePath]
grids = [
    "multilayers/grid-white"
  ]


sorts :: RM [Sort_]
sorts = mapM mkSort grids


mkSort :: String -> RM Sort_
mkSort name = do
    path <- getDataFileName (pngDir </> name <.> "png")
    pixmap <- loadSymmetricPixmap (Position 1 1) path
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
    freeSort = freePixmap . gridPixmap
    size s = pixmapSize $ gridPixmap s

    -- if rendered without scaling, the grid will be multiplied
    renderIconified sort ptr =
        renderPixmapSimple ptr (gridPixmap sort)
    renderEditorObject ptr offset eo = do
        -- render the grid
        resetMatrix ptr
        translate ptr offset
        windowSize <- fmap fromIntegral <$> sizeQPainter ptr
        let sort = editorSort eo
            pix = gridPixmap sort
            positions = calculateGridRenderPositions windowSize (size sort) offset

        forM_ positions $ \ p -> do
            translate ptr p
            renderPixmapSimple ptr pix
            translate ptr (negateAbelian p)

    initialize sort mSpace ep Nothing = do
        let pos = editorPosition2QtPosition sort ep
        return $ Grid (size sort) pos

    immutableCopy = return

    chipmunks = const []

    renderObject o s ptr offset _ =
        renderGrid s offset (position o) <$> (fmap fromIntegral <$> sizeQPainter ptr)


renderGrid :: GSort -> Offset Double -> Position Double -> Size Double -> [RenderPixmap]
renderGrid sort offset position windowSize =
    let pix = gridPixmap sort
        positions = calculateGridRenderPositions windowSize (size sort) (position +~ offset)
    in (flip map) positions $ \ p ->
            RenderPixmap pix (position +~ p) Nothing

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

