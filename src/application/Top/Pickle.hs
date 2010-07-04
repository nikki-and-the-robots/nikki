{-# language ScopedTypeVariables, NamedFieldPuns, ViewPatterns #-}

module Top.Pickle where


import Prelude hiding (readFile, writeFile)

import System
import System.FilePath
import qualified System.IO as IO

import Utils

import Base.Grounds
import Base.Constants

import Object as Object

import Editor.Scene as ES

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

-- | converts an older file format to the newest version
convertToNewest :: String -> String
convertToNewest s = case parse s of
    Just x -> show x
    Nothing -> error "convertToNewest"



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
    mR <- parseSaved (levelNameToFilePath name)
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

    promptForInput :: String -> IO String
    promptForInput p = do
        putStr p
        getLine


writeObjectsToDisk :: FilePath -> (Grounds EditorObject) -> IO ()
writeObjectsToDisk file objects = do
    writeSaved file $ fmap editorObject2PickleObject objects



