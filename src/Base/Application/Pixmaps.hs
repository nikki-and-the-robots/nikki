
-- module for dealing with pixmaps that are needed for the application generally,
-- like menus, general OSDs, etc.

module Base.Application.Pixmaps (
    withApplicationPixmaps,
  ) where


import Data.Map
import Data.Abelian

import Codec.Binary.UTF8.Light

import Control.Exception

import System.FilePath

import Utils

import Base.Paths
import Base.Types
import Base.Constants
import Base.Pixmap
import Base.Font


withApplicationPixmaps :: (ApplicationPixmaps -> IO a) -> RM a
withApplicationPixmaps cmd = do
    pixmaps <- load
    io (cmd pixmaps `finally` free pixmaps)

load :: RM ApplicationPixmaps
load = do
    finished <- fmapM loadOsd finishedMap
    alphaNumericFont <- loadAlphaNumericFont
    return $ ApplicationPixmaps alphaNumericFont finished

loadOsd :: String -> RM Pixmap
loadOsd name = io . loadPixmap zero =<< getDataFileName (pngDir </> "osd" </> name <.> "png")

free :: ApplicationPixmaps -> IO ()
free (ApplicationPixmaps font finished) = do
    fmapM_ freePixmap finished
    freeFont font

finishedMap :: Map LevelResult String
finishedMap = fromList (
    (Passed, "success") :
    (Failed, "failure") :
    [])
