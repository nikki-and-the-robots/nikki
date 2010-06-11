{-# language ScopedTypeVariables, NamedFieldPuns #-}

module Top.Pickle where


import Prelude hiding (readFile, writeFile)

import Utils

import qualified Data.Indexable as I
-- import Data.Indexable hiding (length, toList, findIndices, fromList, empty)
-- import Data.Binary
-- import qualified Data.ByteString.Lazy as ByteString (ByteString, writeFile, readFile)

-- import Codec.Compression.Zlib

-- import Control.Monad
-- import Control.Applicative ((<$>))

import System
import System.FilePath
import qualified System.IO as IO

import Physics.Chipmunk

-- import Base.Sprited
import Base.Grounds
import Base.Constants

import Object.Types as Object

import Game.Scene
import Game.Scene.Types
import Game.Scene.Camera
-- import Game.Modes.Terminal

import Editor.Scene as ES

import Object.Contacts


-- * IO stuff

type SaveType = Grounds PickleObject

-- type FileFormat = ByteString.ByteString
type FileFormat = String


fileToSave :: FileFormat -> Maybe SaveType
fileToSave = readM

saveToFile :: SaveType -> FileFormat
-- saveToFile = compress . encode
saveToFile = show

readFile :: FilePath -> IO FileFormat
-- readFile = ByteString.readFile
readFile = IO.readFile

writeFile :: FilePath -> FileFormat -> IO ()
-- writeFile = ByteString.writeFile
writeFile = IO.writeFile


readSaved :: FilePath -> IO (Maybe SaveType)
readSaved file = (readFile file :: IO FileFormat) ~> fileToSave

writeSaved :: FilePath -> SaveType -> IO ()
writeSaved file level = writeFile file (saveToFile level :: FileFormat)


-- * loading

load :: Maybe String -> IO (Maybe (String, Grounds PickleObject))
load mDefault = do
    args <- getArgs
    case (args, mDefault) of
        ([name], _) -> inner name
        ([], Just name) -> inner name
        ([], Nothing) -> return Nothing
  where
    inner name = do
        objects <- loadByName name
        return $ Just (name, objects)

loadByName :: String -> IO (Grounds PickleObject)
loadByName name = do
    mR <- readSaved (levelNameToFilePath name)
    return $ case mR of
        Just x -> x
        Nothing -> error ("level not readable: " ++ name)


levelNameToFilePath :: String -> FilePath
levelNameToFilePath x = normalise (levelDir </> x <.> "nl")



-- * saving

save :: EditorScene -> IO ()
save scene = do
    IO.hSetBuffering IO.stdin IO.NoBuffering
    s <- wantsToSave
    case s of
        False -> putStrLn "Ok, not saving"
        True -> do
            name <- askWithDefault "level name" (getLevelName scene)
            assertIO (not $ null name) "filename not empty"
            putStr ("saving as " ++ show name ++ "...")
            writeObjectsToDisk (levelNameToFilePath name) (ES.objects scene)
            putStrLn "done"

wantsToSave :: IO Bool
wantsToSave = do
    putStr "Do you want to save this level? (Y/n): "
    c <- getChar
    putStrLn ""
    if c `elem` "\nYy" then
        return True
      else if c `elem` "nN" then
        return False
      else do
        putStrLn "Wrong answer"
        wantsToSave


askWithDefault :: String -> Maybe String -> IO String
askWithDefault prompt vorauswahl =
    case vorauswahl of
        Nothing -> do
            res <- promptForInput (prompt ++ ": ")
            myreturn res vorauswahl
        Just def -> do
            res <- promptForInput (prompt ++ " [" ++ def ++ "] : ")
            myreturn res vorauswahl
  where
    myreturn "" Nothing = do
        putStrLn "Wrong answer"
        askWithDefault prompt vorauswahl
    myreturn "" (Just x) = return x
    myreturn name _ = return name
--     myreturn a b = do
--         es "myreturn" (a, b)

    promptForInput :: String -> IO String
    promptForInput p = do
        putStr p
        getLine


writeObjectsToDisk :: FilePath -> (Grounds EditorObject) -> IO ()
writeObjectsToDisk file objects = do
    writeSaved file $ fmap editorObject2PickleObject objects



