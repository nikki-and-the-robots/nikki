

import Data.List
import Data.Char

import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.IO.Class
import Control.Arrow

import System.Directory hiding (doesFileExist)
import System.IO

import Development.Shake
import Development.Shake.FilePath


-- | Returns all recursive dependencies of the given haskell file.
-- (Recursive module imports will result in an infinite loop.)
getHaskellDeps :: FilePath -> Action [FilePath]
getHaskellDeps file = do
    exists <- doesFileExist file
    when (not exists) $
        error ("file not found: " ++ file)
    rec [] [file]
  where
    rec :: [FilePath] -> [FilePath] -> Action [FilePath]
    rec done [] = return []
    rec done (file : queue) = do
        exists <- doesFileExist file
        if not exists then rec (file : done) queue else do
            files <- extractHaskellDeps file
            let done' = file : done
                queue' = nub (queue ++ filter (not . (`elem` done)) files)
            indirectImports <- rec done' queue'
            return (file : indirectImports)

hsImports :: String -> [String]
hsImports xs = [ takeWhile (\x -> isAlphaNum x || x `elem` "._") $ dropWhile (not . isUpper) x
               | x <- lines xs, "import " `isPrefixOf` x]

extractHaskellDeps :: FilePath -> Action [FilePath]
extractHaskellDeps file = do
    code <- readFile' file
    let imports = hsImports code
    filterM doesFileExist $ map moduleToFile imports

moduleToFile :: String -> FilePath
moduleToFile =
    map (\ c -> if c == '.' then '/' else c) >>>
    (++ ".hs")


pkgs =
    "transformers" :
    "clocked" :
    "mtl" :
    "data-accessor" :
    "Hipmunk" :
    "safe" :
    "text" :
    "bytestring" :
    "deepseq" :
    "binary" :
    "binary-communicator" :
    "aeson" :
    "directory" :
    "temporary" :
    "vector" :
    "download-curl" :
    "strict" :
    "filepath" :
    "email-validate" :
    "bifunctors" :
    "LibZip" :
    "RSA" :
    "sfml-audio" :
    "cmdargs" :
    "data-accessor-mtl" :
    "FindBin" :
    "uniplate" :
    "template" :
    []

addShakeDir = ("shake/" ++)
removeShakeDir f =
    if "shake/" `isPrefixOf` f then
        drop 6 f
      else
        error ("removeShakeDir: path does not start with " ++ show "shake/")


main = do
  hSetBuffering stdout NoBuffering
  shake shakeOptions{shakeParallel = 2, shakeVerbosity = Loud} $ do
    let qtWrapper = "cpp" </> "dist" </> "libqtwrapper.a"
        cppMakefile = "cpp" </> "dist" </> "Makefile"
        ghcFlags =
            "-O0" :
            "-outputdir shake" :
            "-ishake" :
            []
        ghcLinkFlags =
            "-threaded" :
            "-rtsopts" :
            map ("-package=" ++) pkgs ++
            []

    want ["shake/core"]

    "shake/core" *> \ core -> do
        os <- map (addShakeDir . flip replaceExtension "o") <$> getHaskellDeps "Main.hs"
        need (qtWrapper : os)
        let libFlags = ["-lqtwrapper", "-Lcpp/dist", "-lQtGui", "-lQtOpenGL"]
        system' "ghc" $ ["-o",core] ++ libFlags ++ os ++ ghcFlags ++ ghcLinkFlags

    "//*.o" *> \ o -> do
--     objectOrInterfaceFile ?> \ o -> do
        let hsFile = removeShakeDir $ replaceExtension o "hs"
        deps <- extractHaskellDeps hsFile
        need (hsFile : (map (addShakeDir . flip replaceExtension "hi") deps))
        system' "ghc" $ ["-c", hsFile] ++ ghcFlags

    "//*.hi" *> \ hi -> do
        need [replaceExtension hi "o"]

    qtWrapper *> \ _ -> do
        cs <- map ("cpp" </>) <$> getDirectoryFiles "cpp" "*.cpp"
        hs <- map ("cpp" </>) <$> getDirectoryFiles "cpp" "*.h"
        need (cppMakefile : cs ++ hs)
        system' "bash" ("-c" : "cd cpp/dist; make" : [])

    cppMakefile *> \ x -> do
        need ["cpp" </> "CMakeLists.txt"]
        liftIO $ createDirectoryIfMissing False ("cpp" </> "dist")
        system' "bash" ("-c" : "cd cpp/dist; cmake .." : [])


-- * utils

_withDirectory :: MonadIO m => FilePath -> m a -> m a
_withDirectory dir action = do
    outer <- liftIO getCurrentDirectory
    liftIO $ setCurrentDirectory dir
    r <- action
    liftIO $ setCurrentDirectory outer
    return r

objectOrInterfaceFile :: FilePath -> Bool
objectOrInterfaceFile f =
    ext == ".hi" || ext == ".o"
  where
    ext = takeExtension f
