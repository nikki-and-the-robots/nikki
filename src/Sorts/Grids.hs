{-# language MultiParamTypeClasses, DeriveDataTypeable #-}

module Sorts.Grids where


import Data.Data
import Data.Abelian

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
    ("multilayers/skyline-green-far", WrappedHorizon) :
    ("multilayers/skyline-green-near", WrappedHorizon) :
    ("multilayers/stars-top", WrappedHorizon) :
    ("multilayers/plankton-far", Tiled) :
    ("multilayers/plankton-near", Tiled) :
    []


sorts :: [IO (Maybe Sort_)]
sorts =
    map ((Just <$>) . mkSort) grids ++
    map mkStoryModeSort storyModeGrids

mkSort :: (String, GridType) -> IO Sort_
mkSort (name, typ) = do
    path <- getDataFileName (pngDir </> name <.> "png")
    pixmap <- loadSymmetricPixmap (Position 1 1) path
    return $ Sort_ $ GSort typ (SortId ("grid/" ++ name)) pixmap

mkStoryModeSort :: (String, GridType) -> IO (Maybe Sort_)
mkStoryModeSort (name, typ) = do
    path <- getStoryModeDataFileName (pngDir </> name <.> "png")
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
    size s = pixmapSize $ gridPixmap s

    -- if rendered without scaling, the grid will be multiplied
    renderIconified sort ptr =
        renderPixmapSimple ptr (gridPixmap sort)
    renderEditorObject ptr offset eo = do
        -- render the grid
        windowSize <- sizeQPainter ptr
        let debugMode = False

            debugPadding = Position 100 100
            usedOffset = if debugMode
                then offset -~ debugPadding
                else offset
            usedWindowSize = if debugMode
                then windowSize -~ position2size (fmap (* 2) debugPadding)
                else windowSize

            sort = editorSort eo
            pix = gridPixmap sort
            position = epToPosition (size sort) (eo ^. editorPosition)
            positions = calculateGridRenderPositions usedWindowSize usedOffset
                            (gridType sort) (size sort) position

        resetMatrix ptr
        when debugMode $ do
            setPenColor ptr red 1
            drawRect ptr debugPadding usedWindowSize

        translate ptr offset

        forM_ positions $ \ p -> do
            translate ptr p
            when debugMode $
                fillRect ptr zero (size sort) (alpha ^= 0.5 $ yellow)
            renderPixmapSimple ptr pix
            translate ptr (negateAbelian p)


    initialize _app _ _mSpace sort ep Nothing _ = do
        let pos = epToPosition (size sort) ep
        return $ Grid (size sort) pos

    immutableCopy = return

    chipmunks = const []

    isUpdating = const False

    renderObject _ _ o s ptr offset _ =
        renderGrid s offset (position o) <$> sizeQPainter ptr


renderGrid :: GSort -> Offset Double -> Position Double -> Size Double -> [RenderPixmap]
renderGrid sort offset position windowSize =
    let debugMode = False
        debugPadding = Position 100 100
        debugCommands =
            map (\ p ->
                 RenderCommand zero $ \ ptr -> fillRect ptr p (size sort) (alpha ^= 0.5 $ yellow))
                positions ++
            (RenderCommand zero $ \ ptr ->
                resetMatrix ptr *>
                setPenColor ptr red 1 *>
                drawRect ptr debugPadding usedWindowSize) :
            []

        pix = gridPixmap sort
        usedWindowSize = if debugMode
            then Size 500 300
            else windowSize
        usedOffset = if debugMode
            then offset -~ debugPadding
            else offset
        positions = calculateGridRenderPositions usedWindowSize usedOffset
                        (gridType sort) (size sort) position

    in (flip map positions $ \ p -> RenderPixmap pix p Nothing) ++
       if debugMode then debugCommands else [] ++
       []



-- | calculates where to render the grid, so that every space of the screen is filled
calculateGridRenderPositions :: Size Double -> Offset Double -- ^ rendering offset
    -> GridType -> Size Double
    -> Position Double -- ^ actual render position
    -> [Position Double]
calculateGridRenderPositions windowSize offset typ tileSize position =
    map toRenderPosition intPositions
  where
    toRenderPosition :: (Int, Int) -> Position Double
    toRenderPosition (x, y) =
        position +~
        Position (fromIntegral x * width tileSize) (fromIntegral y * height tileSize)
    intPositions :: [(Int, Int)]
    intPositions = cartesian [xmin .. xmax] ys
    ys = case typ of
        Tiled -> [ymin .. ymax]
        WrappedHorizon -> if 0 >= ymin && 0 <= ymax then [0] else []

    xmin = floor ((- positionX offset - positionX position) / width tileSize)
    xmax = floor ((- positionX offset + width windowSize - positionX position) / width tileSize)
    ymin = floor ((- positionY offset - positionY position) / height tileSize)
    ymax = floor ((- positionY offset + height windowSize - positionY position) / height tileSize)
