
-- module for dealing with pixmaps that are needed for the application generally,
-- like menus, general OSDs, etc.

module Base.Application.Pixmaps (
    ApplicationPixmaps(..),
    withApplicationPixmaps,
  ) where


import Data.Map
import Data.Abelian

import Control.Exception

import System.FilePath

import Utils

import Base.Paths
import Base.Types
import Base.Constants
import Base.Pixmap
import Base.Monad


data ApplicationPixmaps = ApplicationPixmaps {
    finished :: Map LevelResult Pixmap
  }

withApplicationPixmaps :: (ApplicationPixmaps -> IO a) -> IO a
withApplicationPixmaps cmd = do
    pixmaps <- load
    cmd pixmaps `finally` free pixmaps

load :: IO ApplicationPixmaps
load = do
    finished <- fmapM loadOsd finishedMap
    return $ ApplicationPixmaps finished

loadOsd :: String -> IO Pixmap
loadOsd name = loadPixmap zero =<< getDataFileName (pngDir </> "osd" </> name <.> "png")

free :: ApplicationPixmaps -> IO ()
free (ApplicationPixmaps finished) =
    fmapM_ freePixmap finished

finishedMap :: Map LevelResult String
finishedMap = fromList (
    (Passed, "success") :
    (Failed, "failure") :
    [])
