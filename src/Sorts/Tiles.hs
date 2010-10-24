{-# language NamedFieldPuns, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable #-}

module Sorts.Tiles (
    sorts,
    tileShapeAttributes,
    mkAllTiles,
  ) where


import Paths_nikki
import Utils

import Data.Abelian
import Data.Generics

import System.FilePath

import Graphics.Qt as Qt

import Physics.Chipmunk as CM

import Base.Constants
import Base.Pixmap
import Base.Types

import Object


-- * Tile configuration

-- all loaded tiles with offset and size
names :: [(String, Qt.Position Int, Size Double)]
names = [
    ("tiles/tile-standard-white", (Position (- 33) (- 33)), Size 64 64),
    ("tiles/tile-standard-black", (Position (- 1) (- 1)), Size 64 64),
    ("tiles/terminal-frame-editor", (Position (- 1) (- 1)), Size 192 192)
  ]

-- | points are moved by this distance to avoid sticky edges
tileMergingEpsilon = 1


-- * Tile loading

sorts :: IO [Sort_]
sorts = do
    mapM (\ (a, b, c) -> mkSort a b c) names

mkSort :: String -> Offset Int -> Size Double -> IO Sort_
mkSort name offset size = do
    pngFile <- getDataFileName (pngDir </> name <.> "png")
    pixmap <- newQPixmap pngFile
    return $ Sort_ $ TSort name (Pixmap pixmap size (fmap fromIntegral offset))

data TSort
    = TSort {
        name :: String,
        tilePixmap :: Pixmap
      }
    deriving (Show, Typeable)


data Tile
    = Tile {
        tchipmunk :: Chipmunk
      }
  deriving (Show, Typeable)


instance Sort TSort Tile where
    sortId TSort{name} = SortId name

    size (TSort _ pix) = pixmapSize pix

    sortRender sort ptr _ =
        renderPixmapSimple ptr (tilePixmap sort)

    initialize sort@TSort{} Nothing editorPosition Nothing = do
        let pos = editorPosition2QtPosition sort editorPosition
        return $ Tile $ (ImmutableChipmunk pos 0 0 [])

    immutableCopy = es "immutableCopy: use AllTiles"
    chipmunks = es "chipmunks: use AllTiles"
    render (Tile (ImmutableChipmunk position _ _ _)) sort ptr offset now = do
        resetMatrix ptr
        translate ptr offset
        let pix = tilePixmap sort
        translate ptr (position +~ pixmapOffset pix)
        drawPixmap ptr zero $ pixmap pix

-- before initializing the scene, all tiles in the physics scene are being merged 
-- (in Top.Initialisation), resulting in an AllTiles object. 
-- This is a workaround for merging tiles. It relies on the following things:
-- 1. Tiles are static
-- 2. Tiles are being rendered above everything else in the physics layer

unwrapTSort :: Sort_ -> TSort
unwrapTSort (Sort_ s) = case cast s of
    Just x -> x


data AllTilesSort
    = AllTilesSort [EditorObject TSort]
  deriving (Show, Typeable)

data AllTiles
    = AllTiles Chipmunk [(TSort, Qt.Position Double)]
  deriving (Show, Typeable)

mkAllTiles :: [EditorObject Sort_] -> EditorObject Sort_
mkAllTiles tiles = EditorObject (Sort_ (AllTilesSort (fmap (fmap unwrapTSort) tiles)))  zero Nothing

instance Sort AllTilesSort AllTiles where
    sortId _ = SortId "allTiles"
    size = error "size: not in use for AllTiles"
    sortRender = error "sortRender: not in use for AllTiles"
    initialize (AllTilesSort editorObjects) (Just space) (EditorPosition 0 0) Nothing = do
        let renderables = map mkRenderable editorObjects
        chipmunks <- initChipmunks space editorObjects
        return $ AllTiles chipmunks renderables

    immutableCopy (AllTiles c x) = do
        c' <- CM.immutableCopy c
        return $ AllTiles c' x

    chipmunks (AllTiles c _) = [c]

    render (AllTiles _ renderables) _ ptr offset now = do
        resetMatrix ptr
        translate ptr offset
        mapM_ draw renderables
      where
        draw (sort, position) = do
            let pixOffset = position +~ pixmapOffset (tilePixmap sort)
            translate ptr pixOffset
            drawPixmap ptr zero $ pixmap $ tilePixmap sort
            translate ptr (negateAbelian pixOffset)

mkRenderable :: EditorObject TSort -> (TSort, Qt.Position Double)
mkRenderable (EditorObject sort ep Nothing) = (sort, editorPosition2QtPosition sort ep)

initChipmunks :: Space -> [EditorObject TSort] -> IO Chipmunk
initChipmunks space objects =
    initShapes space $ mkAbsoluteShapes objects

-- * polygon logick

-- | creates ShapeTypes with absolute coordinates
-- here the actual merging of Tiles takes place
mkAbsoluteShapes :: [EditorObject TSort] -> [ShapeType]
mkAbsoluteShapes =
    map mkAbsoluteShape
    >>> removeStickyEdges tileMergingEpsilon

mkAbsoluteShape :: EditorObject TSort -> ShapeType
mkAbsoluteShape (EditorObject sort ep Nothing) =
    mapVectors (+~ chipmunkPosition) $
    mkRectFromPositions (negateAbelian halfSizeVector) halfSizeVector
  where
    halfSizeVector = Vector (w / 2) (h / 2)
    baryCenterOffset = halfSizeVector
    Size w h = size sort
    chipmunkPosition = qtPosition2Vector (editorPosition2QtPosition sort ep)
        +~ baryCenterOffset

-- * chipmunk stuff

initShapes :: Space -> [ShapeType] -> IO Chipmunk
initShapes space shapeTypes = do
    let shapesWithAttributes = map (mkShapeDescription tileShapeAttributes) shapeTypes
    initChipmunk space (bodyAttributes zero) shapesWithAttributes zero

bodyAttributes :: Vector -> BodyAttributes
bodyAttributes pos =
    StaticBodyAttributes {
        CM.position = pos
      }

tileShapeAttributes :: ShapeAttributes
tileShapeAttributes =
    ShapeAttributes {
        elasticity = 0.5,
        friction = 2,
        CM.collisionType = TileCT
      }
