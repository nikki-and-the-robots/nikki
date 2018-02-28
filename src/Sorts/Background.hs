{-# language ViewPatterns, MultiParamTypeClasses, DeriveDataTypeable #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

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

import           Data.Abelian
import           Data.List
import           Data.Maybe
import           Data.Typeable
import           System.FilePath

import           Base hiding (backgrounds)
import           Graphics.Qt
import qualified Sorts.StoryMode
import           Utils

backgrounds =
    "blue" :
    "greynoise" :
    []


sorts :: [IO (Maybe Sort_)]
sorts =
    public ++ storyMode
  where
    public :: [IO (Maybe Sort_)]
    public = map ((Just <$>) . (Sort_ <$>) . mkSort) backgrounds
    storyMode = map ((fmap Sort_ <$>) . mkStoryModeSort) Sorts.StoryMode.backgrounds

mkSort :: String -> IO BSort
mkSort name = do
                                -- zeropadding :)
    pixmaps <- mapM (loadSymmetricPixmap zero) =<< getPngPaths name
    let sortId = SortId ("background/" ++ name)
    return $ BSort sortId (sortByResolution pixmaps)
  where
    getPngPaths :: String -> IO [FilePath]
    getPngPaths n =
        getDataFiles (pngDir </> "backgrounds" </> n) (Just ".png")

mkStoryModeSort :: String -> IO (Maybe BSort)
mkStoryModeSort name = do
    mPngs <- io $ getStoryModeDataFiles (pngDir </> "backgrounds" </> name) (Just ".png")
    case mPngs of
        Nothing -> return Nothing
        Just pngs -> do
            pixmaps <- mapM (loadSymmetricPixmap zero) pngs
            let sortId = SortId ("story-mode/background/" ++ name)
            return $ Just $ BSort sortId (sortByResolution pixmaps)

sortByResolution :: [Pixmap] -> [Pixmap]
sortByResolution = sortBy (compare `on` (width . pixmapSize))


data BSort = BSort {
    bSortId :: SortId,
    pixmaps :: [Pixmap]
  }
    deriving (Show, Typeable)

instance Sort BSort () where
    sortId = bSortId
    size s = pixmapSize $ head $ pixmaps s -- does not make much sense
                                           -- (except for the cursor and iconified rendering)
    renderIconified sort ptr =
        renderPixmapSimple ptr (head $ pixmaps sort)
    renderEditorObject ptr _offset eo = do
        resetMatrix ptr
        windowSize <- sizeQPainter ptr
        renderWholeScreenPixmap ptr windowSize (pixmaps $ editorSort eo)
    initialize _app _ _mSpace _sort _ep Nothing _ = return ()
    immutableCopy = return
    chipmunks = const []
    isUpdating = const False
    renderObject _ _ _ s ptr offset _ = do
        windowSize <- sizeQPainter ptr
        let mPix = pickWholeScreenPixmap windowSize $ pixmaps s
            pix = fromMaybe (last $ pixmaps s) mPix
            pos = size2position $ fmap (fromIntegral . (round :: Double -> Int) . (/ 2)) (windowSize -~ pixmapSize pix)
        return [RenderPixmap pix (pos -~ offset) Nothing]
