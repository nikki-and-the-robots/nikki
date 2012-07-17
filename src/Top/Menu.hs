
module Top.Menu where


import Data.SelectTree
import Data.List as List

import Text.Logging

import Control.Monad.Error

import System.FilePath

import Utils

import Base

import Editor.Scene (initEditorScene)
import Editor.Menu (editLevel)
import Editor.Pickle
import Editor.Pickle.LevelFile
import Editor.Pickle.LevelLoading

import Top.Game (playLevel)

import Distribution.AutoUpdate
import Distribution.AutoUpdate.MenuItem

import StoryMode.Menus

import LevelServer.Client


-- | top level application state
startAppState :: Application -> AppState
startAppState app = NoGUIAppState $ do
    mLevel <- gets play_level
    play_levelA %= Nothing
    case mLevel of
        Nothing -> return $ mainMenu app 0
        Just file -> io $ play app (mainMenu app 0) <$> mkUnknownLevel file

mainMenu :: Application -> Int -> AppState
mainMenu app ps =
    menuAppState app MainMenu Nothing (
        MenuItem (r $ storyModeMenuItem) (storyMode app (play app) . this) :
        MenuItem (r $ p "community levels") (community app 0 . this) :
        MenuItem (r $ p "options") (generalOptions app 0 . this) :
        MenuItem (r autoUpdateMenuItem) (autoUpdate app . this) :
        MenuItem (r $ p "credits") (credits app . this) :
        MenuItem (r $ p "quit") (const $ FinalAppState) :
        []) ps
  where
    r :: Renderable a => a -> RenderableInstance
    r = renderable

    this :: Int -> AppState
    this = mainMenu app

credits :: Application -> Parent -> AppState
credits app parent = NoGUIAppState $ do
    file <- rm2m $ getDataFileName ("manual" </> "credits" <.> "txt")
    prose <- io $ pFile file
    return $ scrollingAppState app prose parent

community :: Application -> Int -> Parent -> AppState
community app ps parent =
    menuAppState app (NormalMenu (p "community levels") Nothing) (Just parent) (
        MenuItem (p "play levels") (selectLevelPlay app . this) :
        MenuItem (p "download levels") (downloadedLevels app (play app) 0 . this) :
        MenuItem (p "editor") (selectLevelEdit app 0 . this) :
        []) ps
  where
    this ps = community app ps parent

-- | select a saved level.
selectLevelPlay :: Application -> Parent -> AppState
selectLevelPlay app parent = NoGUIAppState $ rm2m $ do
    levelFiles <- lookupPlayableLevels
    scores <- io $ getHighScores
    return $ if null $ ftoList levelFiles then
        message app [p "no levels found :("] parent
      else
        treeToMenu app parent (p "choose a level") (showLevelTreeForMenu scores) levelFiles (play app) 0


selectLevelEdit :: Application -> Int -> Parent -> AppState
selectLevelEdit app ps parent = menuAppState app menuType (Just parent) (
    MenuItem (p "new level") (pickNewLevelEdit app . this) :
    MenuItem (p "edit existing level") (selectExistingLevelEdit app . this) :
    []) ps
  where
    menuType = NormalMenu (p "editor") (Just $ p "create a new level or edit an existing one?")
    this ps = selectLevelEdit app ps parent

pickNewLevelEdit :: Application -> AppState -> AppState
pickNewLevelEdit app parent = NoGUIAppState $ rm2m $ do
    pathToEmptyLevel <- getDataFileName (templateLevelsDir </> "empty.nl")
    templateLevelPaths <- filter (not . ("empty.nl" `List.isSuffixOf`)) <$>
                          getDataFiles templateLevelsDir (Just ".nl")
    return $ menuAppState app menuType (Just parent) (
        map mkMenuItem templateLevelPaths ++
        MenuItem (p "empty level") (const $ edit app parent (TemplateLevel pathToEmptyLevel)) :
        []) 0
  where
    menuType = NormalMenu (p "new level") (Just $ p "choose a template to start from")
    mkMenuItem templatePath =
        MenuItem
            (pVerbatim $ takeBaseName templatePath)
            (const $ edit app parent (TemplateLevel templatePath))

selectExistingLevelEdit app parent = NoGUIAppState $ io $ do
    editableLevels <- lookupUserLevels "your levels"
    return $ if null $ ftoList editableLevels then
        message app [p "no levels found :("] parent
      else
        treeToMenu app parent (p "choose a level to edit") (pVerbatim . (^. labelA))
            editableLevels
            (\ parent chosen -> edit app parent chosen) 0


-- | loads a level and plays it.
play :: Application -> Parent -> LevelFile -> AppState
play app parent levelFile = loadingEditorScene app levelFile parent (playLevel app parent False)

edit :: Application -> Parent -> LevelFile -> AppState
edit app parent levelFile = loadingEditorScene app levelFile parent (editLevel app)

-- | load a level, got to playing state afterwards
-- This AppState is a hack to do things from the logic thread
-- in the rendering thread. Cause Qt's pixmap loading is not threadsafe.
loadingEditorScene :: Application -> LevelFile -> AppState
    -> (EditorScene Sort_ -> AppState) -> AppState
loadingEditorScene app file abortion follower =
    appState (busyMessage $ p "loading...") $ io $ do
        eGrounds <- runErrorT $ loadByFilePath (ftoList $ allSorts app) (getAbsoluteFilePath file)
        case eGrounds of
            Right diskLevel ->
                -- level successfully loaded
                return $ follower $ initEditorScene (allSorts app) file diskLevel
            Left errMsg -> do
                fmapM_ (logg Error) $ fmap getString errMsg
                return $ message app errMsg abortion
