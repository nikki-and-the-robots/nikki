{-# language NamedFieldPuns, ScopedTypeVariables #-}

module Editor.Menu (editLevel) where


import Data.SelectTree
import Data.Indexable (indexA)

import Control.Concurrent
import Control.Monad.State

import System.Directory
import System.FilePath

import Graphics.Qt

import Utils

import Base

import Editor.Scene
import Editor.Scene.Types
import Editor.Pickle

import Top.Game


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
    return $ editorLoop app sceneMVar s

-- | main editor loop
editorLoop :: Application -> MVar (EditorScene Sort_) -> EditorScene Sort_ -> AppState
editorLoop app mvar scene = UnManagedAppState $ do
    config <- getConfiguration
    io $ setDrawingCallbackGLContext (window app) (Just $ render config mvar)
    evalStateT worker scene
  where
    worker :: MM AppState
    worker = do
        updateSceneMVar app mvar
        event <- lift $ waitForAppEvent app
        s <- get
        case (editorMode s, event) of
            (_, Press (KeyboardButton Escape _)) -> return $ editorMenu app mvar s 0
            (NormalMode, Press (KeyboardButton T _)) ->
                -- test the level
                return $ playLevel app (editorLoop app mvar s) s
            (NormalMode, Press (KeyboardButton H _)) ->
                -- test the level with Nikki at cursor position
                return $ playLevel app (editorLoop app mvar s) (setNikkiPosition (cursor s) s)
            _ -> do
                -- other events are handled below (in Editor.Scene)
                eventHandled <- updateEditorScene event
                case (eventHandled, event) of
                    (False, Press _) -> do
                        -- unhandled press event -> help will be displayed
                        scene <- get
                        return $ showEditorHelp app (this scene) scene
                    _ -> worker

    render config sceneMVar ptr = do
        scene <- readMVar sceneMVar
        renderEditorScene ptr app config scene

    this scene = editorLoop app mvar scene


-- | state when pressing Escape during edit mode
editorMenu :: Application -> MVar (EditorScene Sort_)
    -> EditorScene Sort_ -> Int -> AppState
editorMenu app mvar scene ps =
    case editorMode scene of
        NormalMode ->
            menuAppState app (NormalMenu menuTitle $ Just menuSubTitle) (Just $ edit scene)
              (
              lEnterOEM ++
              (
                (p "select object", selectSort app mvar scene 0 . this) :
                (p "edit layers", editLayers app mvar scene 0 . this) :
                (p "activate selection mode (for copy, cut and paste)",
                    const $ edit (toSelectionMode scene)) :
                (p "try playing the level", const $ playLevel app (edit scene) scene) :
                (p "save level", saveLevel app editWithFilePath scene . this) :
                (p "save level and exit editor",
                    saveLevel app (const $ getMainMenu app) scene . this) :
                (p "exit editor without saving", reallyExitEditor app . this) :
              [])) ps
        ObjectEditMode{} -> exitOEM app mvar scene
        SelectionMode{} ->
            menuAppState app (NormalMenu menuTitle $ Just menuSubTitle) (Just (edit scene)) (
                (p "cut selected objects", const $ edit (cutSelection scene)) :
                (p "copy selected objects", const $ edit (copySelection scene)) :
                (p "delete selected objects", const $ edit (deleteSelection scene)) :
                (p "exit selection mode", const $ edit scene{editorMode = NormalMode}) :
                []) ps
  where
    menuTitle = p "editor"
    menuSubTitle = case levelPath scene of
        Nothing -> p "untitled level"
        Just f -> pVerbatim f
    edit :: EditorScene Sort_ -> AppState
    edit s = editorLoop app mvar (updateSelected s)
    this = editorMenu app mvar scene
    -- | edit the scene, but set a given filepath for the level file
    editWithFilePath :: FilePath -> AppState
    editWithFilePath path = edit scene{levelPath = Just path}

    lEnterOEM = case enterOEM app mvar scene of
        Nothing -> []
        Just x -> [(p "edit object", const x)]


saveLevel :: Application -> (FilePath -> AppState) -> EditorScene Sort_
    -> Parent -> AppState
saveLevel app follower EditorScene{levelPath = (Just path), editorObjects_} _parent =
    appState (busyMessage $ p "saving level...") $ io $ do
        writeObjectsToDisk path editorObjects_
        return $ follower path
saveLevel app follower scene@EditorScene{levelPath = Nothing, editorObjects_} parent =
    askString app parent (p "level name") $ \ name -> NoGUIAppState $ io $ do
        levelDirectory <- getSaveLevelDirectory
        let path = levelDirectory </> name <..> "nl"
        exists <- doesFileExist path
        if exists then
            return $ fileExists app this path editorObjects_
          else return $ appState (busyMessage $ p "saving level...") $ io $ do
            writeObjectsToDisk path editorObjects_
            return $ follower path
  where
    this = saveLevel app follower scene parent

fileExists app save path objects =
    menuAppState app menuType (Just save) (
        (p "no", const save) :
        (p "yes", const writeAnyway) :
        []) 0
  where
    menuType = NormalMenu (p "saving level") (Just (pVerbatim path +> p " already exists"))
    writeAnyway = appState (busyMessage $ p "saving level...") $ io $ do
        writeObjectsToDisk path objects
        return $ getMainMenu app

reallyExitEditor :: Application -> Parent -> AppState
reallyExitEditor app editor =
    menuAppState app menuType (Just editor) (
        (p "no", const editor) :
        (p "yes", const $ getMainMenu app) :
        []) 0
  where
    menuType = NormalMenu (p "saving level") (Just $ p "exit without saving?")

selectSort :: Application -> MVar (EditorScene Sort_)
    -> EditorScene Sort_ -> Int -> Parent -> AppState
selectSort app mvar scene ps editorMenu =
    treeToMenu app editorMenu (p "select object")
        (fmap (sortId >>> getSortId) $ scene ^. availableSorts) (const select) ps
  where
    select :: String -> AppState
    select n =
        editorLoop app mvar scene'
      where
        scene' = case selectFirstElement pred (scene ^. availableSorts) of
            Just newTree -> availableSorts ^= newTree $ scene
        pred sort = SortId n == sortId sort


enterOEM :: Application -> MVar (EditorScene Sort_)
    -> EditorScene Sort_ -> Maybe AppState
enterOEM app mvar scene = do -- maybe monad
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
    edit s = editorLoop app mvar s

exitOEM :: Application -> MVar (EditorScene Sort_)
    -> EditorScene Sort_ -> AppState
exitOEM app mvar s =
    editorLoop app mvar s{editorMode = NormalMode}


editLayers :: Application -> MVar (EditorScene Sort_)
    -> EditorScene Sort_ -> Int -> Parent -> AppState
editLayers app mvar scene ps parent =
    menuAppState app (NormalMenu (p "edit layers") Nothing) (Just parent) (
        (p "change layer distance", changeLayerDistance app mvar scene . this) :
        (p "add background layer", edit (addDefaultBackground scene)) :
        (p "add foreground layer", edit (addDefaultForeground scene)) :
        []) ps
  where
    edit s = const $ editorLoop app mvar s
    this ps = editLayers app mvar scene ps parent

changeLayerDistance :: Application -> MVar (EditorScene Sort_)
    -> EditorScene Sort_ -> Parent -> AppState
changeLayerDistance app mvar scene parent =
    askStringRead app parent (p "x distance") $ \ x ->
    askStringRead app parent (p "y distance") $ \ y ->
        editorLoop app mvar
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
            helpText = fmap p $ lines $ oemHelp phantomOEM
        in scrollingAppState app helpText parent
  where
    showHelpFile :: AppState
    showHelpFile = appState (busyMessage (p "showHelpFile (editor)")) $ do
        file <- rm2m $ getDataFileName "manual/editor.txt"
        text <- io $ pFile file
        return $ scrollingAppState app text parent
