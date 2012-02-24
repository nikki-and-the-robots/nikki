

import Safe

import Data.List
import Data.Char

import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.IO.Class
import Control.Arrow

import System.Directory hiding (doesFileExist)
import System.IO
import System.Environment

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


readPackages :: Action [String]
readPackages =
    nub <$> filter (not . null) <$> filter (not . isComment) <$> lines <$>
        readFile' "shakePackages"
  where
    isComment x = headMay x == Just '#'


main = do
  hSetBuffering stdout NoBuffering
  putStrLn "building..."
  [Just mode] <- fmap readMay <$> getArgs
  shake shakeOptions{shakeThreads = 2, shakeVerbosity = Quiet} $ do
    let qtWrapper = "cpp" </> "dist" </> "libqtwrapper.a"
        cppMakefile = "cpp" </> "dist" </> "Makefile"
        ghcFlags =
            optFlag mode :
            ("-outputdir " ++ shakeDir mode) :
            ("-i" ++ shakeDir mode) :
            []
        ghcLinkFlags pkgs =
            optFlag mode :
            "-threaded" :
            "-rtsopts" :
            map ("-package=" ++) pkgs ++
            []

    want [shakeDir mode ++ "/core"]

    (shakeDir mode ++ "/core") *> \ core -> do
        os <- map (addShakeDir mode . flip replaceExtension "o") <$> getHaskellDeps "Main.hs"
        need (qtWrapper : os)
        let libFlags = ["-lqtwrapper", "-Lcpp/dist", "-lQtGui", "-lQtOpenGL"]
        putQuiet ("linking: " ++ core)
        pkgs <- readPackages
        ghc $ ["-o",core] ++ libFlags ++ os ++ ghcFlags ++ ghcLinkFlags pkgs

    ["//*.o", "//*.hi"] *>> \ [o, hi] -> do
        let hsFile = removeShakeDir mode $ replaceExtension o "hs"
        deps <- extractHaskellDeps hsFile
        need (hsFile : (map (addShakeDir mode . flip replaceExtension "hi") deps))
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


ghc args = do
    _ <- askOracle ["ghc-pkg"]
    system' "ghc" args
