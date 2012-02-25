{-# language GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses,
    DeriveDataTypeable #-}


import Safe

import Data.List
import Data.Char
import Data.Binary
import Data.Typeable
import Data.Hashable

import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.IO.Class
import Control.Arrow
import Control.DeepSeq

import System.Directory hiding (doesFileExist)
import System.IO
import System.Environment

import Development.Shake
import Development.Shake.FilePath
import Development.Shake.FileTime


data Mode = Release | Devel
  deriving (Show, Read)

optFlag :: Mode -> String
optFlag Release = "-O2"
optFlag Devel   = "-O0"

shakeDir m = "shake/" ++ show m

addShakeDir mode = ((shakeDir mode ++ "/") ++)
removeShakeDir mode f =
    if prefix `isPrefixOf` f then
        drop (length prefix) f
      else
        error ("removeShakeDir: path does not start with " ++ show prefix)
  where
    prefix = shakeDir mode ++ "/"

main = do
  hSetBuffering stdout NoBuffering
  putStrLn "building..."
  [Just mode] <- fmap readMay <$> getArgs
  shake shakeOptions {
        shakeThreads = 2,
        shakeVerbosity = Quiet
   } $ do
    let qtWrapper = "cpp" </> "dist" </> "libqtwrapper.a"
        cppMakefile = "cpp" </> "dist" </> "Makefile"
        ghcFlags =
            optFlag mode :
            ("-outputdir " ++ shakeDir mode) :
            ("-i" ++ shakeDir mode) :
            []
        ghcLinkFlags =
            optFlag mode :
            "-threaded" :
            "-rtsopts" :
            []

    want [shakeDir mode ++ "/core"]

    (shakeDir mode ++ "/core") *> \ core -> do
        rdeps <- snd <$> apply1 (HaskellTDeps "Main.hs")
        let os = map (addShakeDir mode . (flip replaceExtension "o")) ("Main.hs" : rdeps)
        need (qtWrapper : os)
        let libFlags = ["-lqtwrapper", "-Lcpp/dist", "-lQtGui", "-lQtOpenGL"]
        putQuiet ("linking: " ++ core)
        ghc $ ["-o",core] ++ libFlags ++ os ++ ghcFlags ++ ghcLinkFlags

    ["//*.o", "//*.hi"] *>> \ [o, hi] -> do
        let hsFile = removeShakeDir mode $ replaceExtension o "hs"
        deps <- snd <$> apply1 (HaskellDeps hsFile)
        need (hsFile : (map (addShakeDir mode . (flip replaceExtension "hi")) deps))
        putQuiet ("compiling: " ++ hsFile)
        ghc $ ["-c", hsFile] ++ ghcFlags

    qtWrapper *> \ _ -> do
        cs <- map ("cpp" </>) <$> getDirectoryFiles "cpp" "*.cpp"
        hs <- map ("cpp" </>) <$> getDirectoryFiles "cpp" "*.h"
        need (cppMakefile : cs ++ hs)
        system' "bash" ("-c" : "cd cpp/dist; make" : [])

    cppMakefile *> \ x -> do
        need ["cpp" </> "CMakeLists.txt"]
        liftIO $ createDirectoryIfMissing False ("cpp" </> "dist")
        system' "bash" ("-c" : "cd cpp/dist; cmake .." : [])

    addOracle ["ghc-pkg"] $ do
        (out,err) <- systemOutput "ghc-pkg" ["list","--simple-output"]
        return (words out ++ words err)

    addOracle ["packages"] $ do
        let isComment x = headMay x == Just '#'
        sort <$> nub <$>
            filter (not . null) <$>
            filter (not . isComment) <$>
            lines <$>
            readFile' "shakePackages"

    defaultHaskellTDeps
    defaultHaskellDeps


ghc args = do
    _ <- askOracle ["ghc-pkg"]
    pkgs <- askOracle ["packages"]
    let packageFlags = map ("-package=" ++) pkgs
    system' "ghc" $
        "-hide-all-packages" :
        packageFlags ++
        args



-- * docks

newtype HaskellTDeps = HaskellTDeps FilePath
  deriving (Eq, Show, Binary, NFData, Typeable, Hashable)

instance Rule HaskellTDeps (FileTime, [FilePath]) where
    validStored (HaskellTDeps file) (time, _) =
        (Just time ==) <$> getModTimeMaybe file

defaultHaskellTDeps = defaultRule $ \ (HaskellTDeps haskellFile) -> Just $ do
    directDeps <- snd <$> apply1 (HaskellDeps haskellFile)
    transitiveDeps <- concat <$> map snd <$> mapM apply1 (map (HaskellTDeps . (flip replaceExtension "hs")) directDeps)
    time <- liftIO $ getModTimeError "Error, file does not exist and no rule available:" haskellFile
    return (time, nub $ directDeps ++ transitiveDeps)


newtype HaskellDeps = HaskellDeps FilePath
  deriving (Eq, Show, Binary, NFData, Typeable, Hashable)

instance Rule HaskellDeps (FileTime, [FilePath]) where
    validStored (HaskellDeps file) (time, _) =
        (Just time ==) <$> getModTimeMaybe file

defaultHaskellDeps = defaultRule $ \ (HaskellDeps haskellFile) -> Just $ do
    code <- readFile' haskellFile
    let imports = hsImports code
    deps <- filterM doesFileExist $ map moduleToFile imports
    putQuiet (haskellFile ++ " imports " ++ unwords deps)
    time <- liftIO $ getModTimeError "Error, file does not exist and no rule available:" haskellFile
    return (time, deps)

hsImports :: String -> [String]
hsImports xs = [ takeWhile (\x -> isAlphaNum x || x `elem` "._") $ dropWhile (not . isUpper) x
               | x <- lines xs, "import " `isPrefixOf` x]

moduleToFile :: String -> FilePath
moduleToFile =
    map (\ c -> if c == '.' then '/' else c) >>>
   (<.> "hs")
