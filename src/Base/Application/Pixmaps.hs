
-- module for dealing with pixmaps that are needed for the application generally,
-- like menus, general OSDs, etc.

module Base.Application.Pixmaps (
    loadApplicationPixmaps,
  ) where


import Data.Abelian

import System.FilePath

import Graphics.Qt

import Utils

import Base.Paths
import Base.Types
import Base.Constants
import Base.Pixmap
import Base.Font


loadApplicationPixmaps :: RM ApplicationPixmaps
loadApplicationPixmaps =
    ApplicationPixmaps <$>
        (loadBackground "background") <*>
        (loadBackground "backgroundOverlay") <*>
        loadAlphaNumericFont <*>
        loadDigitFont <*>
        loadHeaderCubePixmaps <*>
        (loadOsd (Position 32 32) (Size 772 268) "menuTitle") <*>
        (loadOsd (Position 32 32) (Size 472 88) "pause") <*>
        (loadOsd (Position 32 32) (Size 664 88) "success") <*>
        (loadOsd (Position 32 32) (Size 600 88) "failure")

loadBackground name =
    mapM (loadSymmetricPixmap zero) =<<
        getDataFiles (osdDir </> name) (Just ".png")

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
