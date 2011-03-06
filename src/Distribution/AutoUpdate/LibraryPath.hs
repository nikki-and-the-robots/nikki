{-# language CPP #-}

-- | Provides an operation that sets "LD_LIBRARY_PATH" to
-- the directory the current executable is in on linux.

module Distribution.AutoUpdate.LibraryPath where


#ifdef linux_HOST_OS

import System.Environment.FindBin

import System.Posix.Env

setLibraryPath :: IO ()
setLibraryPath = do
    progPath <- getProgPath
    let libraryPathKey = "LD_LIBRARY_PATH"
    mOldLibraryPath <- getEnv libraryPathKey
    case mOldLibraryPath of
        Nothing -> setEnv libraryPathKey progPath False
        Just oldValue -> setEnv libraryPathKey (progPath ++ ":" ++ oldValue) True

#else

setLibraryPath :: IO ()
setLibraryPath = return ()

#endif

