{-# language NamedFieldPuns, MultiParamTypeClasses, FlexibleInstances,
    DeriveDataTypeable, ScopedTypeVariables #-}

module Sorts.Tiles (
    sorts,
    tileShapeAttributes,
    mkAllTiles,
  ) where


import Safe

import Data.Abelian
import Data.Generics
import Data.List

import Text.Parsec

import Control.Monad

import System.FilePath

import Graphics.Qt as Qt

import Physics.Chipmunk as CM

import Utils

import Base

import Object


-- * Tile configuration

frameDuration :: Double = 1.0

-- all loaded tiles with offset and size
names :: [(String, Qt.Position Int, Size Double)]
names =
    ("tiles/white-standard", (Position (- 1) (- 1)), Size 64 64) :
    ("tiles/black-standard", (Position (- 1) (- 1)), Size 64 64) :
    ("tiles/aqua-standard", (Position (- 1) (- 1)), Size 64 64) :
    ("tiles/yellow-standard", (Position (- 1) (- 1)), Size 64 64) :
    ("tiles/red-standard", (Position (- 1) (- 1)), Size 64 64) :
    ("tiles/green-standard", (Position (- 1) (- 1)), Size 64 64) :
    ("tiles/pink-standard", (Position (- 1) (- 1)), Size 64 64) :
    ("tiles/white-small", (Position (- 1) (- 1)), Size 32 32) :
    ("tiles/black-small", (Position (- 1) (- 1)), Size 32 32) :
    ("tiles/aqua-small", (Position (- 1) (- 1)), Size 32 32) :
    ("tiles/yellow-small", (Position (- 1) (- 1)), Size 32 32) :
    ("tiles/red-small", (Position (- 1) (- 1)), Size 32 32) :
    ("tiles/green-small", (Position (- 1) (- 1)), Size 32 32) :
    ("tiles/pink-small", (Position (- 1) (- 1)), Size 32 32) :
    []

-- | points are moved by this distance to avoid sticky edges
tileMergingEpsilon = 1


-- * Tile loading

sorts :: RM [Sort_]
sorts = do
    mapM (\ (a, b, c) -> mkSort a b c) names

mkSort :: String -> Offset Int -> Size Double -> RM Sort_
mkSort name offset size = do
    pngFiles <- getFrameFileNames name
    when (null pngFiles) $
        fail ("no png files found for tile: " ++ name)
    pixmaps <- io $ mapM newQPixmap pngFiles
    return $ Sort_ $ TSort name $ map mkPixmap pixmaps
  where
    mkPixmap pixmap = Pixmap pixmap size (fmap fromIntegral offset)

-- | returns the list of filenames for all the frames with the given name
getFrameFileNames :: String -> RM [FilePath]
getFrameFileNames name = do
    -- paths of all pngs in the corresponding directory
    absolutePaths <- getDataFiles (pngDir </> takeDirectory name) (Just ".png")
    -- making them relative again
    let relativePaths = map ((takeDirectory name </>) . takeFileName) absolutePaths
    files <- mapM getDataFileName $
            map (pngDir </>) $
            map third $
            sortBy (withView snd3 compare) $
            filter (\ (candidateName, _, _) -> withView splitDirectories (==) name candidateName) $
            map parsePath relativePaths
    return files
  where
    parsePath :: String -> (String, Maybe Int, FilePath)
    parsePath path = case parse parseTileName "" path of
        Right (a, b) -> (a, b, path)
        x -> error ("unparseable filename: " ++ path)
    parseTileName :: Parsec String () (String, Maybe Int)
    parseTileName = do
        n <- parseName
        i <- parseFrameNumber
        string ".png"
        eof
        return (n, i)

    parseName = do
        a <- name
        r <- many namePart
        return (a ++ concat r)
      where
        name = many1 (noneOf ['_', '.'])
        namePart = try $ do
            char '_'
            a <- letter
            r <- name
            return ('_' : a : r)

    parseFrameNumber :: Parsec String () (Maybe Int)
    parseFrameNumber = optionMaybe $ do
        char '_'
        s <- many1 digit
        return $ readNote "frameNumber" s


data TSort
    = TSort {
        name :: String,
        tilePixmaps :: [Pixmap]
      }
    deriving (Show, Typeable)


data Tile
    = Tile {
        tchipmunk :: Chipmunk
      }
  deriving (Show, Typeable)


instance Sort TSort Tile where
    sortId TSort{name} = SortId name

    size (TSort _ pixmaps) = pixmapSize $ head pixmaps

    sortRender sort ptr _ =
        renderPixmapSimple ptr $ head $ tilePixmaps sort

    initialize sort@TSort{} Nothing editorPosition Nothing = do
        let pos = editorPosition2QtPosition sort editorPosition
        return $ Tile $ (ImmutableChipmunk pos 0 0 [])

    immutableCopy = es "immutableCopy: use AllTiles"
    chipmunks = es "chipmunks: use AllTiles"
    render (Tile (ImmutableChipmunk position _ _ _)) sort ptr offset now = do
        resetMatrix ptr
        translate ptr offset
        let pix = pickAnimationFrame (tilePixmaps sort) [frameDuration] now
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
            let pix = pickAnimationFrame (tilePixmaps sort) [frameDuration] now
                pixOffset = position +~ pixmapOffset pix
            translate ptr pixOffset
            drawPixmap ptr zero $ pixmap pix
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
