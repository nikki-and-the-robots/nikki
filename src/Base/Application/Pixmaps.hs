
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
    menubackgrounds <- mapM (loadPixmap zero) =<< getDataFiles (osdDir </> "background") (Just ".png")
    alphaNumericFont <- loadAlphaNumericFont
    headerCubePixmaps <- loadHeaderCubePixmaps
    menuTitlePixmap <- loadOsd zero "menuTitle"
    finished <- fmapM (loadOsd zero) finishedMap
    return $ ApplicationPixmaps menubackgrounds alphaNumericFont headerCubePixmaps
        menuTitlePixmap finished

loadHeaderCubePixmaps :: RM HeaderCubePixmaps
loadHeaderCubePixmaps = do
    start <- loadOsd off "box-left"
    standard <- loadOsd off "box-standard"
    space <- loadOsd off "box-space"
    end <- loadOsd off "box-right"
    return $ HeaderCubePixmaps start standard space end
  where
    off = Position 1 1

loadOsd :: Position Int -> String -> RM Pixmap
loadOsd offset name = io . loadPixmap offset =<< getDataFileName (osdDir </> name <.> "png")

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

finishedMap :: Map LevelResult String
finishedMap = fromList (
    (Passed, "success") :
    (Failed, "failure") :
    [])
