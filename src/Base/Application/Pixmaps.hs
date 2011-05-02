
-- module for dealing with pixmaps that are needed for the application generally,
-- like menus, general OSDs, etc.

module Base.Application.Pixmaps (
    withApplicationPixmaps,
  ) where


import Data.Map
import Data.Abelian

import Control.Exception

import System.FilePath

import Graphics.Qt

import Utils

import Base.Paths
import Base.Types
import Base.Constants
import Base.Pixmap
import Base.Font
import Base.Renderable.Common


withApplicationPixmaps :: (ApplicationPixmaps -> IO a) -> RM a
withApplicationPixmaps cmd = do
    pixmaps <- load
    io (cmd pixmaps `finally` free pixmaps)

load :: RM ApplicationPixmaps
load = do
    menubackgrounds <- mapM (loadSymmetricPixmap zero) =<<
            getDataFiles (osdDir </> "background") (Just ".png")
    alphaNumericFont <- loadAlphaNumericFont
    headerCubePixmaps <- loadHeaderCubePixmaps
    menuTitlePixmap <- loadOsd (Position 32 32) (Size 772 268) "menuTitle"
    finished <- fmapM loadFinished finishedMap
    return $ ApplicationPixmaps menubackgrounds alphaNumericFont headerCubePixmaps
        menuTitlePixmap finished

loadHeaderCubePixmaps :: RM HeaderCubePixmaps
loadHeaderCubePixmaps = do
    start <- loadOsd (Position 31 31) (Size 48 44) "box-left"
    standard <- loadOsd (Position 1 31) (Size 48 44) "box-standard"
    space <- loadOsd (Position 1 31) (Size 48 44) "box-space"
    end <- loadOsd (Position 1 31) (Size 44 44) "box-right"
    return $ HeaderCubePixmaps start standard space end

loadOsd :: Position Double -> Size Double -> String -> RM Pixmap
loadOsd offset size name = io . loadPixmap offset size =<< getDataFileName (osdDir </> name <.> "png")

osdDir = pngDir </> "osd"

free :: ApplicationPixmaps -> IO ()
free (ApplicationPixmaps menuBackgrounds font cubePixmaps menuTitle finished) = do
    fmapM_ freePixmap menuBackgrounds
    freeFont font
    freeHeaderCubePixmaps cubePixmaps
    freePixmap menuTitle
    fmapM_ freePixmap finished

freeHeaderCubePixmaps (HeaderCubePixmaps a b c d) =
    fmapM_ freePixmap [a, b, c, d]

finishedMap :: Map LevelResult (String, Position Double, Size Double)
finishedMap = fromList (
    (Passed, ("success", Position 32 32, Size 664 88)) :
    (Failed, ("failure", Position 32 32, Size 600 88)) :
    [])

loadFinished :: (String, Position Double, Size Double) -> RM Pixmap
loadFinished (name, offset, size) =
    loadOsd offset size name
