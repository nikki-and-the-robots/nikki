{-# language OverloadedStrings, RecordWildCards #-}

module Editor.Pickle.LevelFile (
    LevelFile(..),
    mkStandardLevel,
    mkUserLevel,
    mkEpisodeLevel,
    mkUnknownLevel,
    getAbsoluteFilePath,
    isUserLevel,
    isTemplateLevel,
    levelMetaData,
    LevelUID,
    levelUID,
    LevelMetaData(..),
    showLevelTreeForMenu,
    showLevelForMenu,
  ) where


import Data.SelectTree
import qualified Data.Map as Map

import System.FilePath
import System.Directory

import Utils

import Base

import StoryMode.Types


mkStandardLevel :: FilePath -> FilePath -> IO LevelFile
mkStandardLevel levelDir levelFile = do
    (dir, package, file) <- mkLevelPath levelDir levelFile
    StandardLevel dir package file <$> loadMetaData levelFile

mkUserLevel :: FilePath -> FilePath -> IO LevelFile
mkUserLevel levelDir levelFile = do
    (dir, package, file) <- mkLevelPath levelDir levelFile
    UserLevel dir package file <$> loadMetaData levelFile

mkEpisodeLevel :: FilePath -> FilePath -> IO (Episode LevelFile -> LevelFile)
mkEpisodeLevel dir_ file_ = do
    (dir, package, file) <- mkLevelPath dir_ file_
    m <- loadMetaData (dir </> package </> file)
    return $ \ e -> EpisodeLevel e dir package file m

-- | PRE: levelDir_ `isPrefixOf` levelFile
mkLevelPath levelDir_ levelFile = do
    dir <- canonicalizePath levelDir_
    let withoutDirM = fmap (dropWhile (`elem` pathSeparators)) $
            dropPrefixMay levelDir_ levelFile
    return $ case withoutDirM of
        Nothing -> error ("mkLevelPath: " ++ show (levelDir_, levelFile))
        Just withoutDir ->
            let package = dropFileName withoutDir
                file = takeFileName withoutDir
            in (dir, package, file)

mkUnknownLevel :: FilePath -> IO LevelFile
mkUnknownLevel = return . UnknownLevelType

isUserLevel :: LevelFile -> Bool
isUserLevel UserLevel{} = True
isUserLevel _ = False

isTemplateLevel :: LevelFile -> Bool
isTemplateLevel TemplateLevel{} = True
isTemplateLevel _ = False

showLevelTreeForMenu :: SelectTree LevelFile -> IO Prose
showLevelTreeForMenu (Leaf label level) = showLevelForMenu level
showLevelTreeForMenu x = return $ pVerbatim (x ^. labelA)

showLevelForMenu :: LevelFile -> IO Prose
showLevelForMenu level = do
    let metaData = levelMetaData level
        name = meta_levelName metaData
    highScores <- getHighScores
    let mHighScore = Map.lookup (levelUID level) highScores
    return $ pVerbatim
        (name ++ " " ++ mkScoreString (meta_numberOfBatteries metaData) mHighScore)
