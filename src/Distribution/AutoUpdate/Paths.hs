
module Distribution.AutoUpdate.Paths where


import Control.Monad

import System.FilePath
import System.Environment.FindBin
import System.Directory
import System.Info


-- | default repo for updates
defaultRepo = "http://updates.joyridelabs.de/nikki/current"

osError msg = error ("unsupported os for updates: " ++ System.Info.os ++ " (" ++ msg ++ ")")

-- | Find the core executable (using FindBin).
-- The core executable has to reside in the same directory as the restarter (called "nikki")
findCoreExecutable :: IO FilePath
findCoreExecutable = do
    path <- getProgPath
    let executable = path </> mkExecutable "core"
    exists <- doesFileExist executable
    when (not exists) $
        error ("file not found: " ++ executable)
    return executable

-- | relative path from the core executable to the data directory
relativeDeployPath :: FilePath
relativeDeployPath = case System.Info.os of
    "linux" -> "."
    "darwin" -> "../.."
    x -> osError "relativeDeployPath"

-- | relative path from the root of the deployed directory to the directory that
-- contains the executables
deployRootToExecutables :: FilePath
deployRootToExecutables = case System.Info.os of
    "linux" -> "."
    "darwin" -> "Contents/MacOS"
    x -> osError "deployRootToExecutables"

restarterExecutable = mkExecutable "nikki"

coreExecutable = mkExecutable "core"

mkExecutable :: String -> String
mkExecutable = case System.Info.os of
    "linux" -> id
--     "mingw32" -> (<.> "exe")
    "darwin" -> id
    x -> osError "mkExecutable"

mkDeployedFolder :: String -> String
mkDeployedFolder = case System.Info.os of
    "darwin" -> (<.> "app")
    x -> osError "mkDeployedFolder"

data Repo = Repo String

-- | Full URLs gets constructed by
-- repo </> "nikki" </> System.Info.os </> given path
mkUrl :: Repo -> FilePath -> String
mkUrl (Repo repo) path =
    repo </>
    repoPath </>
    path

-- | path in a repo to a certain file
repoPath :: FilePath
repoPath = System.Info.os </> System.Info.arch
