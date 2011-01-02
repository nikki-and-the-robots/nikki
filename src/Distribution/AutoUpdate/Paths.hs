
module Distribution.AutoUpdate.Paths where


import Control.Monad

import System.FilePath
import System.Environment.FindBin
import System.Directory
import System.Info


findCoreExecutable :: IO FilePath
findCoreExecutable = do
    path <- getProgPath
    let executable = path </> toExecutable "core"
    exists <- doesFileExist executable
    when (not exists) $
        error ("file not found: " ++ executable)
    return executable

toExecutable = case System.Info.os of
    "linux" -> id
    "mingw32" -> (<.> "exe")
    "darwin" -> id
