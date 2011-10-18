{-# language CPP #-}

module Base.Paths.GetDataFileName where


#ifdef RootInstall


import Utils

import Base.Types

import qualified Paths_nikki


getDataFileName :: FilePath -> RM FilePath
getDataFileName = io . Paths_nikki.getDataFileName


-- RootInstall
#else


import System.Info
import System.FilePath
import System.Environment.FindBin

import Utils

import Base.Types
import Base.Monad
import Base.Configuration


getDataFileName :: FilePath -> RM FilePath
getDataFileName p = do
    inPlace <- asks run_in_place
    if inPlace then
        return (".." </> "data" </> p)
      else do
        progPath <- io getProgPath
        case os of
            "linux" ->
                return (progPath </> "data" </> p)
            "mingw32" ->
                -- works if the application is deployed in one folder
                return (progPath </> "data" </> p)
            "darwin" ->
                -- works if the application is bundled in an app
                return (progPath </> ".." </> "Resources" </> p)
            x -> error ("unsupported os: " ++ os)

-- RootInstall
#endif
