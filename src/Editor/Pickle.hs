{-# language ScopedTypeVariables, NamedFieldPuns, ViewPatterns #-}

module Editor.Pickle where


import Prelude hiding (readFile, writeFile)

import Data.Convertable

import Control.Monad

import qualified System.IO as IO
import System.Directory

import Utils

import Base

import Editor.Pickle.Types

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


parseSaved :: FilePath -> IO (Maybe SaveType)
parseSaved file = (readFile file :: IO FileFormat) >>= return . parse

writeSaved :: FilePath -> SaveType -> IO ()
writeSaved file level = writeFile file (saveToFile level :: FileFormat)


-- * parsing

parse :: FileFormat -> Maybe SaveType
parse (readM -> Just x :: Maybe SaveType) = Just x
parse (readM -> Just x :: Maybe Old2.SaveType) = Just $ convert x
parse (readM -> Just x :: Maybe Old1.SaveType) =
    Just $ convert $ asTypeOf (undefined :: Old2.SaveType) $ convert x
parse _ = Nothing


-- * loading

loadByFilePath :: [Sort_] -> FilePath -> IO (Grounds (EditorObject Sort_))
loadByFilePath allSorts path = do
    exists <- doesFileExist path
    when (not exists) $
        error ("Sorry, the file \"" ++ path ++ "\" does not exist.")
    mR <- parseSaved path
    return $ case mR of
        Just x -> unpickle allSorts x
        Nothing -> error ("Sorry, this file is not a correct nikki level file: " ++ path)


-- * saving

writeObjectsToDisk :: FilePath -> Grounds (EditorObject Sort_) -> IO ()
writeObjectsToDisk file objects = do
    writeSaved file $ pickle objects
