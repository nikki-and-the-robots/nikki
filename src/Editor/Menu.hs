{-# language NamedFieldPuns, ScopedTypeVariables #-}

module Editor.Menu (PlayLevel, editLevel) where


import Data.SelectTree
import Data.Indexable (indexA)

import Control.Concurrent
import Control.Monad.State

import System.Directory
import System.FilePath

import Graphics.Qt

import Utils

import Base

import Object

import Editor.Scene
import Editor.Scene.Types
import Editor.Pickle

import Top.Game


type PlayLevel = Application -> AppState -> EditorScene Sort_ -> AppState

type MM o = StateT (EditorScene Sort_) M o


updateSceneMVar :: Application -> MVar (EditorScene Sort_) -> MM ()
updateSceneMVar app mvar = do
    s <- get
    io $ do
        modifyMVar_ mvar (const $ return s)
        updateGLContext $ window app


-- * menus and states

editLevel :: Application -> EditorScene Sort_ -> AppState
editLevel app s = NoGUIAppState $ io $ do
    sceneMVar <- newMVar s
    return $ editorLoop app playLevel sceneMVar s

-- | main editor loop
editorLoop :: Application -> PlayLevel -> MVar (EditorScene Sort_)
    -> EditorScene Sort_ -> AppState
editorLoop app play mvar scene = GameAppState $ do
    io $ setDrawingCallbackGLContext (window app) (Just $ render mvar)
    evalStateT worker scene
  where
    worker :: MM AppState
    worker = do
        updateSceneMVar app mvar
        event <- lift $ waitForAppEvent app
        s <- get
        case (editorMode s, event) of
            (_, Press k) | isStart k -> return $ editorMenu app play mvar s 0
            (NormalMode, Press (KeyboardButton T _)) ->
                -- test the level
                return $ play app (editorLoop app play mvar s) s
            (NormalMode, Press (KeyboardButton H _)) ->
                -- test the level with Nikki at cursor position
                return $ play app (editorLoop app play mvar s) (setNikkiPosition (cursor s) s)
            _ -> do
                -- other events are handled below (in Editor.Scene)
                eventHandled <- updateEditorScene event
                case (eventHandled, event) of
                    (False, Press _) -> do
                        -- unhandled press event -> help will be displayed
                        scene <- get
                        return $ showEditorHelp app (this scene) scene
                    _ -> worker

    render sceneMVar ptr = do
        scene <- readMVar sceneMVar
        renderEditorScene ptr scene

    this scene = editorLoop app play mvar scene


-- | state when pressing Escape during edit mode
editorMenu :: Application -> PlayLevel -> MVar (EditorScene Sort_)
    -> EditorScene Sort_ -> Int -> AppState
editorMenu app play mvar scene ps =
    case editorMode scene of
        NormalMode ->
            menuAppState app menuTitle (Just $ edit scene)
              (
              lEnterOEM ++
              (
                ("select object", selectSort app play mvar scene 0 . this) :
                ("edit layers", editLayers app play mvar scene 0 . this) :
                ("activate selection mode (for copy, cut and paste)",
                    const $ edit (toSelectionMode scene)) :
                ("try playing the level", const $ play app (edit scene) scene) :
                ("save level", saveLevel app editWithFilePath scene . this) :
                ("save level and exit editor",
                    saveLevel app (const $ getMainMenu app) scene . this) :
                ("exit editor without saving", reallyExitEditor app . this) :
              [])) ps
        ObjectEditMode{} -> exitOEM app play mvar scene
        SelectionMode{} ->
            menuAppState app menuTitle (Just (edit scene)) (
                ("cut selected objects", const $ edit (cutSelection scene)) :
                ("copy selected objects", const $ edit (copySelection scene)) :
                ("delete selected objects", const $ edit (deleteSelection scene)) :
                ("exit selection mode", const $ edit scene{editorMode = NormalMode}) :
                []) ps
  where
    menuTitle = case levelPath scene of
        Nothing -> "editing untitled level"
        Just f -> "editing " ++ f
    edit :: EditorScene Sort_ -> AppState
    edit s = editorLoop app play mvar (updateSelected s)
    this = editorMenu app play mvar scene
    -- | edit the scene, but set a given filepath for the level file
    editWithFilePath :: FilePath -> AppState
    editWithFilePath path = edit scene{levelPath = Just path}

    lEnterOEM = case enterOEM app play mvar scene of
        Nothing -> []
        Just x -> [("edit object", const x)]


saveLevel :: Application -> (FilePath -> AppState) -> EditorScene Sort_
    -> Parent -> AppState
saveLevel app follower EditorScene{levelPath = (Just path), editorObjects_} _parent =
    appState (busyMessage $ p "saving level...") $ io $ do
        writeObjectsToDisk path editorObjects_
        return $ follower path
saveLevel app follower scene@EditorScene{levelPath = Nothing, editorObjects_} parent =
    askString app parent "level name" $ \ name -> NoGUIAppState $ rm2m $ do
        levelDirectory <- getFreeLevelsDirectory
        let path = levelDirectory </> name <..> "nl"
        exists <- io $ doesFileExist path
        if exists then
            return $ fileExists app this path editorObjects_
          else return $ appState (busyMessage $ p "saving level...") $ io $ do
            writeObjectsToDisk path editorObjects_
            return $ follower path
  where
    this = saveLevel app follower scene parent

fileExists app save path objects =
    menuAppState app ("level with the name " ++ path ++ " already exists") (Just save) [
        ("no", const save),
        ("yes", const writeAnyway)
      ] 0
  where
    writeAnyway = appState (busyMessage $ p "saving level...") $ io $ do
        writeObjectsToDisk path objects
        return $ getMainMenu app

reallyExitEditor :: Application_ s -> Parent -> AppState
reallyExitEditor app editor =
    menuAppState app "really exit without saving?" (Just editor) (
        ("no", const editor) :
        ("yes", const $ getMainMenu app) :
        []) 0

selectSort :: Application -> PlayLevel -> MVar (EditorScene Sort_)
    -> EditorScene Sort_ -> Int -> Parent -> AppState
selectSort app play mvar scene ps editorMenu =
    treeToMenu app editorMenu
        (fmap (sortId >>> getSortId) $ scene ^. availableSorts) select ps
  where
    select :: String -> AppState
    select n =
        editorLoop app play mvar scene'
      where
        scene' = case selectFirstElement pred (scene ^. availableSorts) of
            Just newTree -> availableSorts ^= newTree $ scene
        pred sort = SortId n == sortId sort


enterOEM :: Application -> PlayLevel -> MVar (EditorScene Sort_)
    -> EditorScene Sort_ -> Maybe AppState
enterOEM app play mvar scene = do -- maybe monad
    (layerIndex, i) <- selected scene
    selectedObject <- getSelectedObject scene
    _ <- objectEditMode $ editorSort $ selectedObject
    let modObjects = layerA layerIndex ^:
            modifyContent (indexA i .> editorOEMState ^: fmap mod)
        mod :: OEMState -> OEMState
        mod = oemEnterMode scene
    Just $ edit $
        editorObjects ^: modObjects $
        scene{editorMode = ObjectEditMode i}
  where
    edit :: EditorScene Sort_ -> AppState
    edit s = editorLoop app play mvar s

exitOEM :: Application -> PlayLevel -> MVar (EditorScene Sort_)
    -> EditorScene Sort_ -> AppState
exitOEM app play mvar s =
    editorLoop app play mvar s{editorMode = NormalMode}


editLayers :: Application -> PlayLevel -> MVar (EditorScene Sort_)
    -> EditorScene Sort_ -> Int -> Parent -> AppState
editLayers app play mvar scene ps parent =
    menuAppState app "edit layers" (Just parent) (
        ("change layer distance", changeLayerDistance app play mvar scene . this) :
        ("add background layer", edit (addDefaultBackground scene)) :
        ("add foreground layer", edit (addDefaultForeground scene)) :
        []) ps
  where
    edit s = const $ editorLoop app play mvar s
    this ps = editLayers app play mvar scene ps parent

changeLayerDistance :: Application -> PlayLevel -> MVar (EditorScene Sort_)
    -> EditorScene Sort_ -> Parent -> AppState
changeLayerDistance app play mvar scene parent =
    askStringRead app parent "x distance" $ \ x ->
    askStringRead app parent "y distance" $ \ y ->
        editorLoop app play mvar
            (editorObjects .> layerA (selectedLayer scene) ^:
                (setYDistance y . setXDistance x) $ scene)

-- | shows an editor help corresponding to the current editor mode
showEditorHelp :: Application -> AppState -> EditorScene Sort_ -> AppState
showEditorHelp app parent scene = case editorMode scene of
    NormalMode{} -> showHelpFile
    SelectionMode{} -> showHelpFile
    (ObjectEditMode i) ->
        let (Just oem) = objectEditMode $ editorSort $ getMainLayerEditorObject scene i
            phantomOEM :: OEMState = oemInitialize oem undefined
            helpText = proseLines $ p $ oemHelp phantomOEM
        in scrollingAppState app helpText parent
  where
    showHelpFile :: AppState
    showHelpFile = appState (busyMessage (p "showHelpFile (editor)")) $ do
        file <- rm2m $ getDataFileName "manual/editor.txt"
        text <- io $ pFile file
        return $ scrollingAppState app text parent
