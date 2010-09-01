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

-- | converts an older file format to the newest version
convertToNewest :: String -> String
convertToNewest s = case parse s of
    Just x -> show x
    Nothing -> error "convertToNewest"



-- * loading

load :: Maybe String -> IO (Maybe (FilePath, Grounds PickleObject))
load mDefault = do
    args <- getArgs
    case (args, mDefault) of
        ([levelfile], _) -> inner levelfile
        ([], Just levelfile) -> inner levelfile
        ([], Nothing) -> return Nothing
  where
    inner path = do
        objects <- loadByFilePath path
        return $ Just (path, objects)

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

saveDepr :: EditorScene Sort_ -> IO ()
saveDepr scene = do
    IO.hSetBuffering IO.stdin IO.NoBuffering
    s <- wantsToSave
    case s of
        False -> putStrLn "Ok, not saving"
        True -> do
            levelFile <- askWithDefault "level path" $ error "(getLevelPath scene)"
            assertIO (not $ null levelFile) "filename not empty"
            putStr ("saving as " ++ show levelFile ++ "...")
            writeObjectsToDisk levelFile (editorObjects scene)
            putStrLn "done"

wantsToSave :: IO Bool
wantsToSave = do
    c <- promptForInput "Do you want to save this level? (Y/n):"
    if c `elem` ["", "Y", "y"] then
        return True
      else if c `elem` ["n", "N"] then
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

promptForInput :: String -> IO String
promptForInput p = do
    putStrLn p
    getLine


writeObjectsToDisk :: FilePath -> Grounds (EditorObject Sort_) -> IO ()
writeObjectsToDisk file objects = do
    writeSaved file $ fmap editorObject2PickleObject objects



