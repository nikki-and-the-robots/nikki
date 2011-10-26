{-# language ViewPatterns, NamedFieldPuns, ScopedTypeVariables, MultiParamTypeClasses,
    DeriveDataTypeable #-}

module Sorts.Tiles (
    sorts,
    isTileSort,
    tileShapeAttributes,
    mkAllTiles,
    cacheTiles,
  ) where


import Safe

import Data.Abelian
import Data.Generics
import Data.List
import Data.Maybe
import qualified Data.Indexable as I

import Text.Parsec

import Control.Monad

import System.FilePath

import Graphics.Qt as Qt

import Physics.Chipmunk as CM

import Utils

import Base

import qualified Sorts.StoryMode


-- * Tile configuration

defaultFrameTime :: Seconds = 1.0

-- all loaded tiles with offset and size
names :: [(String, Qt.Position Int, Size Double)]
names =
    ("tiles/black-standard", Position 1 1, Size 64 64) :
    ("tiles/white-standard", Position 1 1, Size 64 64) :
    ("tiles/yellow-standard", Position 1 1, Size 64 64) :
    ("tiles/green-standard", Position 1 1, Size 64 64) :
    ("tiles/aqua-standard", Position 1 1, Size 64 64) :
    ("tiles/blue-standard", Position 1 1, Size 64 64) :
    ("tiles/pink-standard", Position 1 1, Size 64 64) :
    ("tiles/red-standard", Position 1 1, Size 64 64) :
    ("tiles/black-small", Position 1 1, Size 32 32) :
    ("tiles/white-small", Position 1 1, Size 32 32) :
    ("tiles/yellow-small", Position 1 1, Size 32 32) :
    ("tiles/green-small", Position 1 1, Size 32 32) :
    ("tiles/aqua-small", Position 1 1, Size 32 32) :
    ("tiles/blue-small", Position 1 1, Size 32 32) :
    ("tiles/pink-small", Position 1 1, Size 32 32) :
    ("tiles/red-small", Position 1 1, Size 32 32) :
    ("terminals/terminal-standard-bottom-end", Position 1 1, Size 192 32) :
    ("tutorial/robots-01", Position 1 1, Size 416 268) :
    ("tutorial/robots-02", Position 1 1, Size 920 180) :
    ("tutorial/nikki-01", Position 1 1, Size 768 160) :
    ("tutorial/nikki-02", Position 1 1, Size 768 160) :
    ("tutorial/nikki-03", Position 1 1, Size 768 160) :
    ("tutorial/nikki-04", Position 1 1, Size 768 160) :
    []

-- | points are moved by this distance to avoid sticky edges
tileMergingEpsilon = 1


-- * Tile loading

sorts :: RM [Sort_]
sorts = do
    freeSorts <- mapM (\ (a, b, c) -> mkSort False a b c defaultFrameTime Nothing) names
    storyModeSorts <- mapM (\ (a, b, c, frameTime, frameOrder) ->
        mkSort True a b c frameTime frameOrder) Sorts.StoryMode.tiles
    return $ catMaybes (freeSorts ++ storyModeSorts)

-- | returns Nothing if a story mode tile is not available
mkSort :: Bool -> String -> Offset Int -> Size Double -> Seconds -> Maybe [Int]
    -> RM (Maybe Sort_)
mkSort storyMode name offset size frameDuration frameOrder = do
    mPngFiles <- getFrameFileNames storyMode name
    case mPngFiles of
        Nothing -> return Nothing
        Just pngFiles -> do
            when (null pngFiles) $
                fail ("no png files found for tile: " ++ name)
            let sortID = if storyMode then ("story-mode/" ++ name) else name
            frames <- reorderFrames <$> mapM mkTilePixmap pngFiles
            return $ Just $ Sort_ $ TSort sortID frameDuration frames
  where
    mkTilePixmap file = loadPixmap (fmap fromIntegral offset) size file

    reorderFrames :: [Pixmap] -> [Pixmap]
    reorderFrames pixmaps = maybe pixmaps
        (map (\ i -> atNote (note i) pixmaps i)) frameOrder
    note i = name ++ " has no frame with number " ++ show i

-- | Returns the list of filenames for all the frames with the given name
-- Returns Nothing in case a story mode tile is not available.
getFrameFileNames :: Bool -> String -> RM (Maybe [FilePath])
getFrameFileNames storyMode name = do
    -- paths of all pngs in the corresponding directory
    mAbsolutePaths <- getPngFiles storyMode name
    case mAbsolutePaths of
        Nothing -> return Nothing
        Just absolutePaths -> do
            -- making them relative again
            let relativePaths = map ((takeDirectory name </>) . takeFileName) absolutePaths
            files <- mapM (getPngFileName storyMode) $
                    map (pngDir </>) $
                    map third $
                    sortBy (compare `on` snd3) $
                    filter (\ (candidateName, _, _) -> on (==) splitDirectories name candidateName) $
                    map parsePath relativePaths
            return $ Just $ catMaybes files
  where
    parsePath :: String -> (String, Maybe Int, FilePath)
    parsePath path = case parse parseTileName "" path of
        Right (a, b) -> (a, b, path)
        x -> error ("unparseable filename: " ++ path)
    parseTileName :: Parsec String () (String, Maybe Int)
    parseTileName = do
        n <- parseName
        i <- parseFrameNumber
        ignore $ string ".png"
        eof
        return (n, i)

    parseName = do
        a <- name
        r <- many namePart
        return (a ++ concat r)
      where
        name = many1 (noneOf ['_', '.'])
        namePart = try $ do
            ignore $ char '_'
            a <- letter
            r <- name
            return ('_' : a : r)

    parseFrameNumber :: Parsec String () (Maybe Int)
    parseFrameNumber = optionMaybe $ do
        ignore $ char '_'
        s <- many1 digit
        return $ readNote "frameNumber" s

-- | returns all png files in the directory where the tile pngs should be.
-- Returns Nothing in case a storymode tile is loaded, but the story mode is not available.
getPngFiles :: Bool -> String -> RM (Maybe [FilePath])
getPngFiles False name =
    Just <$> getDataFiles (pngDir </> takeDirectory name) (Just ".png")
getPngFiles True name =
    io $ getStoryModeDataFiles (pngDir </> takeDirectory name) (Just ".png")

getPngFileName :: Bool -> FilePath -> RM (Maybe FilePath)
getPngFileName False file = Just <$> getDataFileName file
getPngFileName True file = io $ getStoryModeDataFileName file


data TSort
    = TSort {
        name :: String,
        frameDuration :: Seconds,
        tilePixmaps :: [Pixmap]
      }
    deriving (Show, Typeable)

isTileSort :: Sort s o => s -> Bool
isTileSort (cast -> Just _ :: Maybe TSort) = True
isTileSort (cast -> Just (Sort_ inner) :: Maybe Sort_) = isTileSort inner
isTileSort _ = False

data Tile
    = Tile {
        tchipmunk :: Chipmunk
      }
  deriving (Show, Typeable)


instance Sort TSort Tile where
    sortId TSort{name} = SortId name

    freeSort = fmapM_ freePixmap . nub . tilePixmaps

    size (TSort _ _ pixmaps) = pixmapSize $ head pixmaps

    renderIconified sort ptr =
        renderPixmapSimple ptr $ head $ tilePixmaps sort

    initialize app _ Nothing sort@TSort{} editorPosition Nothing _ = do
        let pos = epToPosition (size sort) editorPosition

--         let (shapes, baryCenterOffset) = mkShapes $ size sort
--             shapesWithAttributes = map (mkShapeDescription shapeAttributes) shapes
--             position = position2vector (epToPosition sort editorPosition)
--                             +~ baryCenterOffset
--             bodyAttributes = mkBodyAttributes shapes position (boxMass sort)
--         chip <- CM.initChipmunk space bodyAttributes
--                     shapesWithAttributes baryCenterOffset
--         return $ Box chip


        return $ Tile $ (ImmutableChipmunk pos 0 0 [])

    immutableCopy = es "immutableCopy: use AllTiles"
    chipmunks = es "chipmunks: use AllTiles"
    renderObject _ _ (Tile (ImmutableChipmunk position _ _ _)) sort _ offset now = return $
        return $ RenderPixmap pix position Nothing
      where
        pix = pickAnimationFrame (tilePixmaps sort) [frameDuration sort] now

-- before initializing the scene, all tiles in the physics scene are being merged 
-- (in Top.Initialisation), resulting in an AllTiles object. 
-- This is a workaround for merging tiles. It relies on the following things:
-- 1. Tiles are static
-- 2. Tiles are being rendered above everything else in the physics layer

unwrapTSort :: Sort_ -> Maybe TSort
unwrapTSort (Sort_ s) = cast s

unwrapTSortEditorObject :: EditorObject Sort_ -> Maybe (EditorObject TSort)
unwrapTSortEditorObject (EditorObject sort pos oem) =
    case unwrapTSort sort of
        Just tsort -> Just $ EditorObject tsort pos oem
        Nothing -> Nothing

data AllTilesSort
    = AllTilesSort [EditorObject TSort]
  deriving (Show, Typeable)

data AllTiles
    = AllTiles Chipmunk [(TSort, Qt.Position Double)]
  deriving (Show, Typeable)

mkAllTiles :: [EditorObject Sort_] -> EditorObject Sort_
mkAllTiles tiles =
    EditorObject (Sort_ (AllTilesSort (catMaybes (fmap unwrapTSortEditorObject tiles)))) zero Nothing

instance Sort AllTilesSort AllTiles where
    sortId _ = SortId "allTiles"
    freeSort = error "freeSort: not in use for AllTiles"
    size = error "size: not in use for AllTiles"
    renderIconified = error "renderIconified: not in use for AllTiles"
    initialize app _ (Just space) (AllTilesSort editorObjects) (EditorPosition 0 0) Nothing
      cachedTiles = io $ do
        let renderables = map mkRenderable editorObjects
        chipmunks <- initChipmunks space cachedTiles editorObjects
        return $ AllTiles chipmunks renderables

    immutableCopy (AllTiles c x) = do
        c' <- CM.immutableCopy c
        return $ AllTiles c' x

    chipmunks (AllTiles c _) = [c]

    renderObject _ _ (AllTiles _ renderables) sort _ _ now = return $
        fmap inner renderables
      where
        inner (sort, pos) = RenderPixmap
            (pickAnimationFrame (tilePixmaps sort) [frameDuration sort] now)
            pos
            Nothing

mkRenderable :: EditorObject TSort -> (TSort, Qt.Position Double)
mkRenderable (EditorObject sort ep Nothing) = (sort, epToPosition (size sort) ep)

initChipmunks :: Space -> CachedTiles -> [EditorObject TSort] -> IO Chipmunk
initChipmunks space cachedTiles objects =
    initShapes space $ mkAbsoluteShapes cachedTiles objects

-- * polygon logick

-- | creates ShapeTypes with absolute coordinates
-- here the actual merging of Tiles takes place
mkAbsoluteShapes :: CachedTiles -> [EditorObject TSort] -> [ShapeType]
mkAbsoluteShapes Nothing =
    map mkAbsoluteShape
    >>> removeStickyEdges tileMergingEpsilon
mkAbsoluteShapes (Just x) = const x

mkAbsoluteShape :: EditorObject TSort -> ShapeType
mkAbsoluteShape (EditorObject sort ep Nothing) =
    mapVectors (+~ chipmunkPosition) $
    mkRectFromPositions (negateAbelian halfSizeVector) halfSizeVector
  where
    halfSizeVector = size2vector $ fmap (/ 2) $ size sort
    baryCenterOffset = halfSizeVector
    chipmunkPosition = position2vector (epToPosition (size sort) ep)
        +~ baryCenterOffset

-- * caching

cacheTiles :: I.Indexable (EditorObject Sort_) -> [ShapeType]
cacheTiles ixs =
    let tiles = catMaybes $ I.toList $ fmap unwrapTSortEditorObject ixs
    in mkAbsoluteShapes Nothing tiles

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
        friction = 0.95,
        CM.collisionType = TileCT
      }
