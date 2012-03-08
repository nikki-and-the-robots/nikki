{- language #-}


import Safe

import Data.List

import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.IO.Class

import System.Directory hiding (doesFileExist)
import System.IO
import System.Environment

import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Imports


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
--         shakeReport = Just "shakeProf"
   } $ do

    want [shakeDir mode ++ "/core"]

    importsDefaultHaskell ["."]

    let ghcFlags =
            optFlag mode :
            ("-outputdir " ++ shakeDir mode) :
            ("-i" ++ shakeDir mode) :
            []
        ghcLinkFlags =
            optFlag mode :
            ("-L" ++ addShakeDir mode "cpp") :
            "-threaded" :
            "-rtsopts" :
            []
    (shakeDir mode ++ "/core") *> \ core -> do
        rdeps <- transitiveImports "Main.hs"
        let os = map (addShakeDir mode . (<.> "o")) ("Main.hs" : rdeps)
        need (qtWrapper mode : os)
        let libFlags = ["-lqtwrapper", "-Lcpp/dist", "-lQtGui", "-lQtOpenGL"]
        putQuiet ("linking: " ++ core)
        ghc $ ["-o",core] ++ libFlags ++ os ++ ghcFlags ++ ghcLinkFlags

    ["//*.hs.o", "//*.hi"] *>> \ [o, hi] -> do
        let hsFile = removeShakeDir mode $ dropExtension o
        deps <- directImports hsFile
        need (hsFile : (map (addShakeDir mode . (flip replaceExtension "hi")) deps))
        putQuiet ("compiling: " ++ hsFile)
        ghc $ ["-c", hsFile, "-o", o] ++ ghcFlags

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

    cppBindings mode


ghc args = do
    _ <- askOracle ["ghc-pkg"]
    pkgs <- askOracle ["packages"]
    let packageFlags = map ("-package=" ++) pkgs
    system' "ghc" $
        "-hide-all-packages" :
        packageFlags ++
        args


-- * cpp stuff

qtWrapper :: Mode -> FilePath
qtWrapper mode = addShakeDir mode ("cpp" </> "libqtwrapper.a")

cppCompileFlags mode =
    "-I/usr/include/qt4" :
    "-I/usr/include/qt4/QtGui" :
    "-I/usr/include/qt4/QtOpenGL" :
    optFlag mode :
    []

cppBindings mode = do

    importsDefaultCpp ["cpp"]

    qtWrapper mode *> \ lib -> do
        cppFiles <- map ("cpp" </>) <$> getDirectoryFiles "cpp" "*.cpp"
        headerFiles <- map ("cpp" </>) <$> getDirectoryFiles "cpp" "*.h"
        let os =
                map (addShakeDir mode)
                (map (<.> ".o") cppFiles ++ map (<.> ".moc.cpp.o") headerFiles)
        need os
        putQuiet ("packing " ++ lib)
        system' "ar" ("rc" : lib : os)

    let isNonMocObjectFile f =
            (".cpp.o" `isSuffixOf` f) &&
            not (".h.moc.cpp.o" `isSuffixOf` f)

    isNonMocObjectFile ?> \ objectFile -> do
        let cppFile = dropExtension $ removeShakeDir mode objectFile
        need [cppFile]
        depends <- directImports cppFile
        need depends
        putQuiet ("compiling: " ++ cppFile)
        system' "g++" ("-c" : cppFile : "-o" : objectFile : cppCompileFlags mode)

    "//*.h.moc.cpp.o" *> \ objectFile -> do
        let cppFile = dropExtension objectFile
        need [cppFile]
        depends <- directImports cppFile
        need depends
        putQuiet ("compiling: " ++ cppFile)
        system' "g++" ("-c" : cppFile : "-o" : objectFile : cppCompileFlags mode)

    "//*.h.moc.cpp" *> \ cppFile -> do
        let headerFile = removeShakeDir mode $ dropExtension $ dropExtension cppFile
        need [headerFile]
        system' "moc" [headerFile, "-o", cppFile]
