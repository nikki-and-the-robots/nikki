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
import System.Directory

import Utils


getDataFileName :: FilePath -> IO FilePath
getDataFileName p = do
    cabalFileExists <- doesFileExist "nikki.cabal" -- fixme: remove reader
    if cabalFileExists then
        return (".." </> "data" </> p)
      else do
        progPath <- getProgPathOrCurrentDirectory
        case os of
            "linux" ->
                -- works if the application is deployed in one folder
                return (progPath </> "data" </> p)
            "mingw32" ->
                -- works if the application is deployed in one folder
                return (progPath </> "data" </> p)
            "darwin" ->
                -- works if the application is bundled in an app
                return (progPath </> ".." </> "Resources" </> p)
            _ -> error ("unsupported os: " ++ os)

-- RootInstall
#endif
