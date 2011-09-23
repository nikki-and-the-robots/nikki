{-# language MultiParamTypeClasses, DeriveDataTypeable #-}

module Sorts.Grids where


import Data.Generics
import Data.Abelian
import Data.Maybe

import System.FilePath

import Graphics.Qt

import Utils

import Base


grids :: [(FilePath, GridType)]
grids =
    ("multilayers/grid-white", Tiled) :
    ("multilayers/steps-white", Tiled) :
    ("multilayers/bricks-white", Tiled) :
    []

storyModeGrids :: [(FilePath, GridType)]
storyModeGrids =
    ("multilayers/skyline-blue-far", WrappedHorizon) :
    ("multilayers/skyline-blue-near", WrappedHorizon) :
    []


sorts :: RM [Sort_]
sorts = do
    gs <- mapM mkSort grids
    storyModeGrids <- mapM mkStoryModeSort storyModeGrids
    return (gs ++ catMaybes storyModeGrids)

mkSort :: (String, GridType) -> RM Sort_
mkSort (name, typ) = do
    path <- getDataFileName (pngDir </> name <.> "png")
    pixmap <- loadSymmetricPixmap (Position 1 1) path
    return $ Sort_ $ GSort typ (SortId ("grid/" ++ name)) pixmap

mkStoryModeSort :: (String, GridType) -> RM (Maybe Sort_)
mkStoryModeSort (name, typ) = do
    mPath <- io $ getStoryModeDataFileName (pngDir </> name <.> "png")
    case mPath of
        Nothing -> return Nothing
        Just path -> do
            pixmap <- loadSymmetricPixmap (Position 1 1) path
            return $ Just $ Sort_ $
                GSort typ (SortId ("story-mode/grid/" ++ name)) pixmap

data GSort = GSort {
    gridType :: GridType,
    sortId_ :: SortId,
    gridPixmap :: Pixmap
  }
    deriving (Show, Typeable)

data Grid = Grid {
    gridSize :: Size Double,
    position :: (Position Double)
  }
    deriving (Show, Typeable)

data GridType = Tiled | WrappedHorizon
  deriving Show

instance Sort GSort Grid where
    sortId = sortId_
    freeSort = freePixmap . gridPixmap
    size s = pixmapSize $ gridPixmap s

    -- if rendered without scaling, the grid will be multiplied
    renderIconified sort ptr =
        renderPixmapSimple ptr (gridPixmap sort)
    renderEditorObject ptr offset eo = do
        -- render the grid
        resetMatrix ptr
        translate ptr offset
        windowSize <- sizeQPainter ptr
        let sort = editorSort eo
            pix = gridPixmap sort
            positions = calculateGridRenderPositions windowSize
                            (gridType sort) (size sort) offset

        translate ptr (Position 0 (- height (size sort)))
        forM_ positions $ \ p -> do
            translate ptr p
            renderPixmapSimple ptr pix
            translate ptr (negateAbelian p)
--         fillRect ptr zero (size sort) (alpha ^= 0.5 $ red)


    initialize app _ mSpace sort ep Nothing _ = do
        let pos = epToPosition (size sort) ep
        return $ Grid (size sort) pos

    immutableCopy = return

    chipmunks = const []

    renderObject _ _ o s ptr offset _ =
        renderGrid s offset (position o) <$> sizeQPainter ptr


renderGrid :: GSort -> Offset Double -> Position Double -> Size Double -> [RenderPixmap]
renderGrid sort offset position windowSize =
    let pix = gridPixmap sort
        positions = calculateGridRenderPositions windowSize
                        (gridType sort) (size sort) (position +~ offset)
    in (flip map positions $ \ p -> RenderPixmap pix (position +~ p) Nothing)
--         +: RenderCommand position
--             (\ ptr -> fillRect ptr zero (size sort) (alpha ^= 0.5 $ red))


-- | calculates where to render the grid, so that every space of the screen is filled
calculateGridRenderPositions :: Size Double -> GridType -> Size Double
    -> Position Double -- ^ actual render position
    -> [Position Double]
calculateGridRenderPositions windowSize typ tileSize position =
    map toRenderPosition intPositions
  where
    toRenderPosition :: (Int, Int) -> Position Double
    toRenderPosition (x, y) = 
        Position (fromIntegral x * width tileSize) (fromIntegral y * height tileSize)
    intPositions :: [(Int, Int)]
    intPositions = case typ of
        Tiled -> cartesian [xmin .. xmax] [ymin .. ymax]
        WrappedHorizon -> map (\ x -> (x, 0)) [xmin .. xmax]

    xmin = floor ((0 - positionX position) / width tileSize)
    xmax = floor ((width windowSize - positionX position) / width tileSize)
    ymin = floor ((0 - positionY position) / height tileSize)
    ymax = floor ((height windowSize - positionY position) / height tileSize)
