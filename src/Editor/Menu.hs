{-# language NamedFieldPuns, ScopedTypeVariables #-}

module Editor.Menu (PlayLevel, editorLoop) where


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


type PlayLevel = Application -> AppState -> EditorScene Sort_ -> AppState

type MM o = StateT (EditorScene Sort_) M o


updateSceneMVar :: Application -> MVar (EditorScene Sort_) -> MM ()
updateSceneMVar app mvar = do
    s <- get
    io $ do
        modifyMVar_ mvar (const $ return s)
        updateGLContext $ window app


-- * menus and states

editorLoop :: Application -> PlayLevel -> MVar (EditorScene Sort_)
    -> EditorScene Sort_ -> AppState
editorLoop app play mvar scene = AppState (rt "editorLoop") $ do
    io $ setDrawingCallbackGLContext (window app) (Just $ render mvar)
    evalStateT worker scene
  where
    worker :: MM AppState
    worker = do
        updateSceneMVar app mvar
        event <- lift $ waitForAppEvent app
        s <- get
        case (editorMode s, event) of
            (_, Press k) | isStart k -> return $ editorMenu app play mvar s
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
    -> EditorScene Sort_ -> AppState
editorMenu app play mvar scene =
    case editorMode scene of
        NormalMode ->
            menu app menuTitle (Just (edit scene))
              (
              lEnterOEM ++
              [
                ("select object", selectSort app this play mvar scene),
                ("edit layers", editLayers app play mvar scene),
                ("activate selection mode (for copy, cut and paste)", edit (toSelectionMode scene)),
                ("try playing the level", play app (edit scene) scene),
                ("save level", saveLevel app this editWithFilePath scene),
                ("save level and exit editor", saveLevel app this (const $ getMainMenu app) scene),
                ("exit editor without saving", reallyExitEditor app this)
              ])
        ObjectEditMode{} -> exitOEM app play mvar scene
        SelectionMode{} ->
            menu app menuTitle (Just (edit scene)) [
                ("cut selected objects", edit (cutSelection scene)),
                ("copy selected objects", edit (copySelection scene)),
                ("delete selected objects", edit (deleteSelection scene)),
                ("exit selection mode", edit scene{editorMode = NormalMode})
              ]
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
        Just x -> [("edit object", x)]


saveLevel :: Application -> AppState -> (FilePath -> AppState) -> EditorScene Sort_ -> AppState
saveLevel app _parent follower EditorScene{levelPath = (Just path), editorObjects_} =
    ioAppState (rt "saveLevel") $ do
        writeObjectsToDisk path editorObjects_
        return $ follower path
saveLevel app parent follower scene@EditorScene{levelPath = Nothing, editorObjects_} =
    askString app parent "level name" $ \ name -> AppState (rt "saveLevel") $ rm2m $ do
        levelDirectory <- getFreeLevelsDirectory
        let path = levelDirectory </> name <..> "nl"
        exists <- io $ doesFileExist path
        if exists then
            return $ fileExists app this path editorObjects_
          else do
            io $ writeObjectsToDisk path editorObjects_
            return $ follower path
  where
    this = saveLevel app parent follower scene

fileExists app save path objects =
    menu app ("level with the name " ++ path ++ " already exists") (Just save) [
        ("no", save),
        ("yes", writeAnyway)
      ]
  where
    writeAnyway = ioAppState (rt "writeAnyway") $ do
        writeObjectsToDisk path objects
        return $ getMainMenu app

reallyExitEditor app editor =
    menu app "really exit without saving?" (Just editor) [
        ("no", editor),
        ("yes", getMainMenu app)
      ]

selectSort :: Application -> AppState -> PlayLevel -> MVar (EditorScene Sort_)
    -> EditorScene Sort_ -> AppState
selectSort app editorMenu play mvar scene =
    treeToMenu app editorMenu (fmap (sortId >>> getSortId) $ scene ^. availableSorts) select
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
    -> EditorScene Sort_ -> AppState
editLayers app play mvar scene =
    menu app "edit layers" (Just editMenu) [
        ("change layer distance", changeLayerDistance app this scene edit),
        ("add background layer", edit (addDefaultBackground scene)),
        ("add foreground layer", edit (addDefaultForeground scene))
      ]
  where
    edit s = editorLoop app play mvar s
    editMenu = editorMenu app play mvar scene
    this = editLayers app play mvar scene

changeLayerDistance :: Application -> AppState -> EditorScene Sort_ -> (EditorScene Sort_ -> AppState) -> AppState
changeLayerDistance app parent scene follower =
    askStringRead app parent "x distance" $ \ x ->
    askStringRead app parent "y distance" $ \ y -> AppState (rt "changeLayerDistance") $
        return $ follower
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
    showHelpFile = appState (rt "showHelpFile (editor)") $ do
        file <- rm2m $ getDataFileName "manual/editor.txt"
        text <- io $ pFile file
        return $ scrollingAppState app text parent
