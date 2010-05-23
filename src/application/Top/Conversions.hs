{-# language ScopedTypeVariables, NamedFieldPuns #-}

module Top.Conversions where


import Prelude hiding (readFile, writeFile)

import Utils

import qualified Data.Indexable as I
import Data.Indexable hiding (length, toList, findIndices, fromList, empty)
-- import Data.Binary
-- import qualified Data.ByteString.Lazy as ByteString (ByteString, writeFile, readFile)

-- import Codec.Compression.Zlib

import Control.Monad
import Control.Applicative ((<$>))

import System
import System.FilePath
import qualified System.IO as IO

import Game.Scene
import Game.Scene.Camera
import Base.Grounds
import Objects.Types as Objects
import Objects.Collisions
import Game.Modes.Terminal

import Base.Sprited
import Editor.Scene as ES
import Editor.Binary ()


mkScene :: Grounds UninitializedObject -> IO UninitializedScene
mkScene objects = do
    let nikki = single "savedToScene" $ I.findIndices isNikki $ mainLayerIndexable objects
    osdSpriteds <- initialOsdSpriteds
    return $ Scene 0 0 objects nikki initialCameraState nikki emptyCollisions Nothing osdSpriteds


-- * IO stuff

type SaveType = Grounds UnloadedEObject

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

load :: Maybe String -> IO (Maybe (String, Grounds UnloadedEObject))
load mDefault = do
    args <- getArgs
    case (args, mDefault) of
        ([name], _) -> Just <$> loadByName name
        ([], Just name) -> Just <$> loadByName name
        ([], Nothing) -> return Nothing

loadByName :: String -> IO (String, Grounds UnloadedEObject)
loadByName name = do
    level <- readSaved (levelNameToFilePath name)
--     let level :: SaveType = read c
--     test level
    let (Just grounds@(Grounds backgrounds mainLayer foregrounds)) = level
    mapM_ checkNormalized [backgrounds, foregrounds]
    mapM_ (checkNormalized . content) (I.toList backgrounds ++ [mainLayer] ++ I.toList foregrounds)

    return (name, grounds)

-- | checks, if the given (Indexable a) is normalized
checkNormalized :: Indexable a -> IO ()
checkNormalized i =
    when (not (isNormalized i)) $
        error "please normalize levels when saving"


levelNameToFilePath :: String -> FilePath
levelNameToFilePath x = normalise ("levels" </> x <.> "nl")



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
            innerSave (levelNameToFilePath name) scene
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


innerSave :: FilePath -> EditorScene -> IO ()
innerSave file scene = do
    let level = toSavableLevel scene
    writeSaved file level

-- | converts to UnloadedSpriteds and normalizes the (Indexable a)s.
toSavableLevel :: EditorScene -> Grounds UnloadedEObject
toSavableLevel scene =
    let Grounds backgrounds mainLayer@Layer{content} foregrounds =
            fmap (fmap loaded2UnloadedSprited) (ES.objects scene)
        (content', fun) = normalize content
        backgrounds' = normalizeLayersDisregardingFunction backgrounds
        foregrounds' = normalizeLayersDisregardingFunction foregrounds
    in fun $ Grounds backgrounds' mainLayer{content = content'} foregrounds'
        -- attention: not all indices point to the main layer
        -- (currently yes, but this may change)

-- | normalizes every (Indexable a) in the Layers (both hierarchy levels), but disregards the resulting
-- (index changing) functions
normalizeLayersDisregardingFunction :: Indexable (Layer a) -> Indexable (Layer a)
normalizeLayersDisregardingFunction ixs =
    myFst $ normalize $ fmap (modifyContent (myFst . normalize)) ixs
  where
    -- needed for polymorphic return result of myFst (syb stuff)
    myFst :: (a, () -> ()) -> a
    myFst = fst



