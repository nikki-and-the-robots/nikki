{-# language ViewPatterns #-}

module Editor.Pickle (
    module Editor.Pickle,
    module Editor.Pickle.Types,
  ) where


import Prelude hiding (readFile, writeFile)
import Safe

import Data.Convertable

import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans.Error (ErrorList(..))

import qualified System.IO as IO
import System.Directory

import Utils

import Base

import Sorts.Tiles

import Editor.Pickle.Types
import Editor.Pickle.MetaData

import qualified Legacy.Old1 as Old1
import qualified Legacy.Old2 as Old2


-- * IO stuff

saveToFile :: SaveType -> FileFormat
-- saveToFile = compress . encode
saveToFile = show

readFile :: FilePath -> IO FileFormat
-- readFile = ByteString.readFile
readFile = IO.readFile

writeFile :: FilePath -> FileFormat -> IO ()
-- writeFile = ByteString.writeFile
writeFile = IO.writeFile


parseSaved :: FilePath -> ErrorT [Prose] IO SaveType
parseSaved file = io (readFile file :: IO FileFormat) >>= parse file

writeSaved :: FilePath -> SaveType -> IO ()
writeSaved file level = writeFile file (saveToFile level :: FileFormat)


-- * parsing

parse :: FilePath -> FileFormat -> ErrorT [Prose] IO SaveType
parse _ (readMay -> Just x :: Maybe SaveType) = return x
parse _ (readMay -> Just x :: Maybe Old2.SaveType) = return $ convert x
parse _ (readMay -> Just x :: Maybe Old1.SaveType) =
    return $ convert $ asTypeOf (undefined :: Old2.SaveType) $ convert x
parse path _ =
    throwError (p "Sorry, this file is not a correct nikki level file:" : pv path : [])


-- * loading

instance ErrorList Prose where
    listMsg = error "Editor.Pickle.listMsg"

loadByFilePath :: [Sort_] -> FilePath
    -> ErrorT [Prose] IO DiskLevel
loadByFilePath allSorts path = do
    exists <- io $ doesFileExist path
    when (not exists) $
        throwError (p "file not found:" : pv path : [])
    x <- parseSaved path
    ErrorT $ return $ unpickle allSorts x


-- * saving

writeObjectsToDisk :: FilePath -> LevelMetaData -> Grounds (EditorObject Sort_) -> IO ()
writeObjectsToDisk file metaData objects = do
    let cachedTiles = cacheTiles (objects ^. mainLayer .> content)
        diskLevel = DiskLevel objects (Just cachedTiles)
    writeSaved file $ pickle diskLevel
    saveMetaData file metaData
