{-# language ForeignFunctionInterface, ScopedTypeVariables #-}

-- | works around a bug in FindBin affecting linux.

module System.Environment.FindBin.Fixed where


import Text.Logging

import System.Info
import System.FilePath
import System.Directory
import System.Environment
import qualified System.Environment.FindBin

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable

import Utils

-- | this works around the issue of hackage's FindBin, but it doesn't work
-- when the program is searched executed via $PATH.
getProgPath :: IO FilePath
getProgPath = log =<< case System.Info.os of
    os | os `elem` ("linux" : "darwin" : []) -> do
        dir <- getCurrentDirectory
        prog <- getProgName
        (fullProgName : _) <- wrap getFullProgArgv
        takeDirectory <$> canonicalizePath (dir </> fullProgName)
    _ -> System.Environment.FindBin.getProgPath
  where
    log x = logInfo ("getProgPath: " ++ x) >> return x

wrap :: (Ptr CInt -> Ptr (Ptr CString) -> IO ()) -> IO [String]
wrap action = alloca $ \ argcPtr -> alloca $ \ argvArrayPtr -> do
    action argcPtr argvArrayPtr
    argc <- peek argcPtr
    argvArray <- peek argvArrayPtr
    argv :: [CString] <- peekArray (fromIntegral argc) argvArray
    mapM peekCString argv

foreign import ccall unsafe "getFullProgArgv"
  getFullProgArgv :: Ptr CInt -> Ptr (Ptr CString) -> IO ()

foreign import ccall unsafe "getProgArgv"
  getProgArgv :: Ptr CInt -> Ptr (Ptr CString) -> IO ()
