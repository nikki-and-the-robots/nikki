{-# language ScopedTypeVariables, NamedFieldPuns #-}

module Editor.Menu (editLevel) where


import Safe

import Data.SelectTree
import Data.Indexable (indexA)

import Control.Concurrent
import Control.Monad.State

import System.Directory

import Graphics.Qt

import Utils

import Base

import Sorts.Nikki (uniqueNikki)
import Sorts.Battery (countBatteries)

import Editor.Scene
import Editor.Scene.Types
import Editor.Pickle
import Editor.Pickle.LevelFile
import Editor.Pickle.LevelLoading

import LevelServer.Client

import Top.Game


type MM o = StateT (EditorScene Sort_) M o


updateSceneMVar :: Application -> MVar (EditorScene Sort_) -> MM ()
updateSceneMVar app mvar = do
    s <- get
    io $ do
        modifyMVar_ mvar (const $ return s)
        updateMainWindow $ window app


-- * menus and states

editLevel :: Application -> EditorScene Sort_ -> AppState
editLevel app s = NoGUIAppState $ io $ do
    sceneMVar <- newMVar s
    return $ editorLoop app sceneMVar s

-- | main editor loop
editorLoop :: Application -> MVar (EditorScene Sort_) -> EditorScene Sort_ -> AppState
editorLoop app mvar scene = UnManagedAppState $ do
    config <- getConfiguration
    io $ setDrawingCallbackMainWindow (window app) (Just $ render config mvar)
    evalStateT worker scene
  where
    worker :: MM AppState
    worker = do
        updateSceneMVar app mvar
        event <- lift $ waitForAppEvent app
        s <- get
        case (editorMode s, event) of
            (_, Press (KeyboardButton Escape _ _)) -> return $ editorMenu app mvar s 0
            (NormalMode, Press (KeyboardButton T _ _)) ->
                -- test the level
                return $ playLevel app (editorLoop app mvar s) True $
                    cachedTiles ^= Nothing $
                    s
            (NormalMode, Press (KeyboardButton H _ _)) ->
                -- test the level with Nikki at cursor position
                return $ playLevel app (editorLoop app mvar s) True $
                    cachedTiles ^= Nothing $
                    setNikkiPosition (cursor s) $
                    editorObjects ^: uniqueNikki app $
                    s
            _ -> do
                -- other events are handled below (in Editor.Scene)
                config <- lift $ get
                eventHandled <- updateEditorScene config app event
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
                MenuItem (p "select object") (selectSort app mvar scene 0 . this) :
                MenuItem (p "edit layers") (editLayers app mvar scene 0 . this) :
                MenuItem
                    (p "activate selection mode (for copy, cut and paste)")
                    (const $ edit (toSelectionMode scene)) :
                MenuItem (p "try playing the level") (const $ playLevel app (edit scene) True $
                        cachedTiles ^= Nothing $ scene) :
                MenuItem (p "editor tutorial (opens browser)") (openUrl app editorTutorialUrl . this) :
                MenuItem (p "save level") (saveLevel app editWithFilePath scene . this) :
                MenuItem (p "save & upload") (\ ps -> saveAndUpload app scene (this ps) editWithFilePath) :
                MenuItem
                    (p "save level & exit editor")
                    (saveLevel app (const $ getMainMenu app) scene . this) :
                MenuItem (p "exit editor without saving") (reallyExitEditor app . this) :
              [])) ps
        ObjectEditMode{} -> exitOEM app mvar scene
        SelectionMode{} ->
            menuAppState app (NormalMenu menuTitle $ Just menuSubTitle) (Just (edit scene)) (
                MenuItem (p "cut selected objects") (const $ edit (cutSelection scene)) :
                MenuItem (p "copy selected objects") (const $ edit (copySelection scene)) :
                MenuItem (p "delete selected objects") (const $ edit (deleteSelection scene)) :
                MenuItem (p "exit selection mode") (const $ edit scene{editorMode = NormalMode}) :
                []) ps
  where
    menuTitle = p "editor"
    menuSubTitle = case editorLevelFile scene of
        TemplateLevel{} -> p "untitled level"
        f -> pv $ meta_levelName $ levelMetaData f
    edit :: EditorScene Sort_ -> AppState
    edit s = editorLoop app mvar (updateSelected s)
    this = editorMenu app mvar scene
    -- | edit the scene, but set a given filepath for the level file
    editWithFilePath :: LevelFile -> AppState
    editWithFilePath editorLevelFile = edit scene{editorLevelFile}

    lEnterOEM = case enterOEM app mvar scene of
        Nothing -> []
        Just x -> [MenuItem (p "edit object") (const x)]


saveAndUpload app scene parent afterSaveFollower =
    saveLevel app (\ file -> uploadLevel app (afterSaveFollower file) file 0) scene parent


saveLevel :: Application -> (LevelFile -> AppState) -> EditorScene Sort_
    -> Parent -> AppState
saveLevel app follower EditorScene{editorLevelFile, editorObjects_} parent
  | isUserLevel editorLevelFile =
    completeMetaData app parent editorObjects_ (Just $ levelMetaData editorLevelFile) $
      \ metaData ->
        let path = getAbsoluteFilePath editorLevelFile
        in appState (busyMessage $ p "saving level...") $ io $ do
            writeObjectsToDisk path metaData editorObjects_
            return $ follower editorLevelFile{levelMetaData_ = metaData}
saveLevel app follower scene@EditorScene{editorLevelFile, editorObjects_} parent
  | isTemplateLevel editorLevelFile =
    completeMetaData app parent editorObjects_ Nothing $
    \ metaData@(LevelMetaData name _ _ _ _) ->
      NoGUIAppState $ io $ do
        levelDirectory <- getSaveLevelDirectory
        let levelFile = UserLevel levelDirectory "" (name <..> "nl") metaData
            path = getAbsoluteFilePath levelFile
        exists <- doesFileExist path
        if exists then
            return $ fileExists app this path metaData editorObjects_
          else return $ appState (busyMessage $ p "saving level...") $ io $ do
            writeObjectsToDisk path metaData editorObjects_
            return $ follower levelFile{levelMetaData_ = metaData}
  where
    this = saveLevel app follower scene parent

-- | completes the needed metadata
completeMetaData :: Sort s o => Application -> Parent
    -> Grounds (EditorObject s) -> Maybe LevelMetaData
    -> (LevelMetaData -> AppState) -> AppState
completeMetaData a pa objects mMetaData f = case mMetaData of
    Nothing ->
        askString a pa (p "level name") $ \ name ->
        completeAgain $ Just $ LevelMetaData name Nothing Nothing Nothing Nothing
    Just (LevelMetaData name Nothing basedOn batteries musicFile) ->
        askString a pa (p "author name") $ \ author ->
        completeAgain $ Just $ LevelMetaData name (Just author) basedOn batteries musicFile
    Just (LevelMetaData name author basedOn Nothing musicFile) ->
        let numberOfBatteries = countBatteries $
                fmap editorSort $
                (objects ^. mainLayer .> content)
        in completeAgain $ Just $ LevelMetaData name author basedOn
                (Just numberOfBatteries) musicFile
    Just m@(LevelMetaData _ (Just _) _ (Just _) _) ->
        f m
  where
    completeAgain metaData = completeMetaData a pa objects metaData f


fileExists app save path metaData objects =
    menuAppState app menuType (Just save) (
        MenuItem (p "no") (const save) :
        MenuItem (p "yes") (const writeAnyway) :
        []) 0
  where
    menuType = NormalMenu (p "saving level") (Just (pVerbatim path <> p " already exists"))
    writeAnyway = appState (busyMessage $ p "saving level...") $ io $ do
        writeObjectsToDisk path metaData objects
        return $ getMainMenu app

reallyExitEditor :: Application -> Parent -> AppState
reallyExitEditor app editor =
    menuAppState app menuType (Just editor) (
        MenuItem (p "no") (const editor) :
        MenuItem (p "yes") (const $ getMainMenu app) :
        []) 0
  where
    menuType = NormalMenu (p "saving level") (Just $ p "exit without saving?")

selectSort :: Application -> MVar (EditorScene Sort_)
    -> EditorScene Sort_ -> Int -> Parent -> AppState
selectSort app mvar scene ps editorMenu =
    treeToMenu app editorMenu (p "select object") (return . p . (^. labelA))
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
        MenuItem (p "change layer distance") (changeLayerDistance app mvar scene . this) :
        MenuItem (p "add on top of current layer") (edit (addDefaultLayerOnTop scene)) :
        MenuItem (p "add behind current layer") (edit (addDefaultLayerBehind scene)) :
        MenuItem (p "delete current layer") (edit (updateSelected $ deleteCurrentLayer scene)) :
        []) ps
  where
    edit s = const $ editorLoop app mvar s
    this ps = editLayers app mvar scene ps parent

changeLayerDistance :: Application -> MVar (EditorScene Sort_)
    -> EditorScene Sort_ -> Parent -> AppState
changeLayerDistance app mvar scene parent =
    askStringParse app parent (p "x distance") readEither $ \ x ->
    askStringParse app parent (p "y distance") readEither $ \ y ->
        editorLoop app mvar
            (editorObjects .> layerA (scene ^. selectedLayer) ^:
                (setYDistance y . setXDistance x) $ scene)
  where
    readEither :: String -> Either [Prose] Double
    readEither s = maybe (Left [p "not an integer: " <> pv s]) Right (readMay s)

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
