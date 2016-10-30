{-# language ViewPatterns #-}

-- | scripting stuff

module Utils.Scripting where

import           Control.Arrow
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Char
import           Data.List
import           Data.Version
import           Safe
import           System.Directory
import           System.Environment.FindBin
import           System.Exit
import           System.FilePath
import           System.IO
import           System.Process
import           Text.ParserCombinators.ReadP (readP_to_S)

(<~>) :: String -> String -> String
a <~> b = a ++ " " ++ b

-- unix style slashes
(<//>) :: String -> String -> String
a <//> b =
    stripEndSlashes a ++ "/" ++ stripStartSlashes b
  where
    stripStartSlashes = dropWhile (== '/')
    stripEndSlashes = reverse . stripStartSlashes . reverse

stripWhiteSpaces :: String -> String
stripWhiteSpaces = dropWhile isSpace . reverse . dropWhile isSpace . reverse


switchOffCaching =
    mapM_ (\ h -> hSetBuffering h NoBuffering) [stdout, stderr]


-- | displays a message and waits for the return key.
waitForReturn msg = do
    emptyStdin
    putStrLn msg
    _ <- getLine
    return ()
  where
    emptyStdin = do
        r <- hReady stdin
        when r $ do
            _ <- hGetChar stdin
            emptyStdin


-- | executes a unix command on the shell and exits if it does not succeed.
trySystem :: String -> IO ()
trySystem cmd = do
    putStrLn ("Executing \"" ++ cmd ++ "\" ...")
    exitcode <- system cmd
    case exitcode of
        ExitSuccess -> return ()
        ExitFailure n -> exitWith $ ExitFailure n

-- | Returns the program path or the current directory.
getProgPathOrCurrentDirectory :: IO FilePath
getProgPathOrCurrentDirectory = do
    progPath <- getProgPath
    exists <- doesDirectoryExist progPath
    wd <- getCurrentDirectory
    return $ if exists 
        then progPath
        else wd

-- | changes the working directory temporarily.
withCurrentDirectory :: FilePath -> IO () -> IO ()
withCurrentDirectory path cmd = do
    bracket first finish (const cmd)
  where
    first = do
        oldWorkingDirectory <- getCurrentDirectory
        setCurrentDirectory path
        return oldWorkingDirectory
    finish = setCurrentDirectory

-- | copy a whole directory recursively
-- excluding hidden files
-- give full paths to both directories, e.g. (copyDirectory "src/dir" "dest/dir")
copyDirectory :: FilePath -> FilePath -> IO ()
copyDirectory src dst = do
    allFiles <- getFilesRecursive src
    forM_ allFiles copy
  where
    copy file = do
        createDirectoryIfMissing True (takeDirectory (dst </> file))
        copyFile (src </> file) (dst </> file)

-- | returns all (unhidden) files in a directory recursively, sorted.
-- Omits the directories.
getFilesRecursive :: FilePath -> IO [FilePath]
getFilesRecursive root =
    map normalise <$> inner "."
  where
    inner dir = do
        content <- map (dir </>) <$> getFiles (root </> dir) Nothing
        (directories, files) <- partitionM (doesDirectoryExist . (root </>)) content
        recursive <- mapM inner $ directories
        return $ fileSort (files ++ concat recursive)

partitionM :: (a -> IO Bool) -> [a] -> IO ([a], [a])
partitionM p (a : r) = do
    condition <- p a
    (yes, no) <- partitionM p r
    return $ if condition then
        (a : yes, no)
      else
        (yes, a : no)
partitionM _ [] = return ([], [])

-- | removes file and directories if they exist
removeIfExists :: FilePath -> IO ()
removeIfExists f = liftIO $ do
    isFile <- doesFileExist f
    isDirectory <- doesDirectoryExist f
    if isFile then
        removeFile f
      else if isDirectory then
        removeDirectoryRecursive f
      else
        return ()

-- | Returns all files and directories in a given directory, sorted.
-- Omit "." and "..".
getDirectoryRealContents :: FilePath -> IO [FilePath]
getDirectoryRealContents path =
    liftIO $ fileSort <$> filter isContent <$> getDirectoryContents path
  where
    isContent "." = False
    isContent ".." = False
    isContent _ = True

-- | Returns if a path starts with a dot.
isHiddenOnUnix :: FilePath -> Bool
isHiddenOnUnix = headMay >>> (== Just '.')

-- | Returns all unhidden (unix) files in a given directory.
-- @getFiles dir (Just extension)@ returns all files with the given extension.
getFiles :: FilePath -> Maybe String -> IO [FilePath]
getFiles dir mExtension =
    fileSort <$> filter hasRightExtension <$> filter (not . isHiddenOnUnix) <$>
        getDirectoryRealContents dir
  where
    hasRightExtension :: FilePath -> Bool
    hasRightExtension = case mExtension of
        (Just ('.' : '.' : r)) -> error ("don't give extensions that start with two dots: " ++ r)
        (Just extension@('.' : _)) -> takeExtension >>> (== extension)
        (Just extension) -> takeExtension >>> (== ('.' : extension))
        Nothing -> const True

-- | Sorts files case insensitively
fileSort :: [String] -> [String]
fileSort = sortBy (\ a b -> compare (map toUpper $ a) (map toUpper b))

-- | Checks if a directory or file exists and returns (Just file) in that case.
maybeExists :: FilePath -> IO (Maybe FilePath)
maybeExists file = do
    fileExists <- doesFileExist file
    dirExists <- doesDirectoryExist file
    return $ if fileExists || dirExists
        then Just file
        else Nothing

-- | asserts that a file or directory exists.
assertExistance :: FilePath -> IO ()
assertExistance file = do
    isFile <- doesFileExist file
    isDir <- doesDirectoryExist file
    when (not (isFile || isDir)) $
        error ("file not found: " ++ file)

-- | asserts that a file or directory does not exist.
assertNonExistance :: FilePath -> IO ()
assertNonExistance file = do
    isFile <- doesFileExist file
    isDir <- doesDirectoryExist file
    when (isFile || isDir) $
        error ("file already exists: " ++ file)


-- * version stuff

parseVersion :: String -> Either String Version
parseVersion (stripWhiteSpaces -> s) =
    case readP_to_S Data.Version.parseVersion s of
        (last -> (v, "")) -> Right v
        x -> Left ("version parse error: " ++ show (s, x))


-- | reads the nikki version from a given executable
readNikkiVersion :: FilePath -> IO Version
readNikkiVersion nikkiExecutable = do
    -- get missing configuration warnings out of the way
    _ <- readProcess nikkiExecutable ["--version"] ""
    versionString <- readProcess nikkiExecutable ["--version"] ""
    return $ case parse versionString of
        Left errMsg -> error errMsg
        Right version -> version
  where
    parse :: String -> Either String Version
    parse s | prefix `isPrefixOf` s && suffix `isSuffixOf` s =
        Utils.Scripting.parseVersion versionString
      where
        prefix = "Nikki and the Robots ("
        suffix = ")\n"
        versionString = drop (length prefix) $ reverse $ drop (length suffix) $ reverse s
    parse s = Left ("unparseable progamm summary: " ++ show s)
