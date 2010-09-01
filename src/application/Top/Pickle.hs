{-# language ScopedTypeVariables, NamedFieldPuns, ViewPatterns #-}

module Top.Pickle where


import Prelude hiding (readFile, writeFile)

import Control.Monad

import System
import qualified System.IO as IO
import System.Directory

import Utils

import Base.Grounds
import Base.Types

import Object as Object

import Top.Pickle.Types
import qualified Top.Pickle.Old1 as Old1


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
parseSaved file = (readFile file :: IO FileFormat) >>= fromPure parse

writeSaved :: FilePath -> SaveType -> IO ()
writeSaved file level = writeFile file (saveToFile level :: FileFormat)


-- * parsing

parse :: FileFormat -> Maybe SaveType
parse (readM -> Just x :: Maybe SaveType) = Just x
parse (readM -> Just x :: Maybe Old1.SaveType) = Just $ Old1.convert x
parse _ = Nothing


-- * loading

loadByFilePath :: FilePath -> IO (Grounds PickleObject)
loadByFilePath path = do
    exists <- doesFileExist path
    when (not exists) $
        error ("Sorry, the file \"" ++ path ++ "\" does not exist.")
    mR <- parseSaved path
    return $ case mR of
        Just x -> x
        Nothing -> error ("Sorry, this file is not a correct nikki level file: " ++ path)


-- * saving

writeObjectsToDisk :: FilePath -> Grounds (EditorObject Sort_) -> IO ()
writeObjectsToDisk file objects = do
    writeSaved file $ fmap editorObject2PickleObject objects
