{-# language NamedFieldPuns #-}

module Editor.Menu (PlayLevel, editorLoop) where


import Data.Indexable (modifyByIndex)
import Data.SelectTree

import Control.Concurrent
import Control.Monad.State

import System.Directory

import Graphics.Qt

import Utils

import Base.Types
import Base.Events
import Base.Grounds
import Base.Application hiding (selected)

import Object

import Editor.Scene
import Editor.Scene.Types
import Editor.Pickle


type PlayLevel = Application -> AppState -> EditorScene Sort_ -> AppState

type MM o = StateT (EditorScene Sort_) IO o


updateSceneMVar :: Application -> MVar (EditorScene Sort_) -> MM ()
updateSceneMVar app mvar = do
    s <- get
    liftIO $ do
        swapMVar mvar s
        updateAppWidget $ window app


-- * menus and states

editorLoop :: Application -> AppState -> PlayLevel -> MVar (EditorScene Sort_)
    -> EditorScene Sort_ -> AppState
editorLoop app mainMenu play mvar scene = AppState $ do
    setDrawingCallbackAppWidget (window app) (Just $ render mvar)
    evalStateT worker scene
  where
    worker :: MM AppState
    worker = do
        updateSceneMVar app mvar
        event <- liftIO $ waitForAppEvent $ keyPoller app
        s <- get
        if event == Press StartButton then
            return $ editorMenu app mainMenu play mvar s
          else case (editorMode s, event) of
            (NormalMode, Press (KeyboardButton T _)) ->
                return $ play app (editorLoop app mainMenu play mvar s) s
            _ -> do
                -- other events are handled below (in Editor.Scene)
                modifyState (updateEditorScene event)
                worker

    render sceneMVar ptr = do
        scene <- readMVar sceneMVar
        renderEditorScene ptr scene



editorMenu :: Application -> AppState -> PlayLevel -> MVar (EditorScene Sort_)
    -> EditorScene Sort_ -> AppState
editorMenu app mainMenu play mvar scene =
    case editorMode scene of
        NormalMode ->
            menu app (Just menuTitle) (Just (edit scene))
              (
              lEnterOEM ++
              [
                ("select object", selectSort app mainMenu this play mvar scene),
                ("edit layers", editLayers app mainMenu play mvar scene),
                ("activate selection mode (for copy, cut and paste)", edit (toSelectionMode scene)),
                ("try playing the level", play app (edit scene) scene),
                ("return to editing", edit scene),
                ("save level and exit editor", saveLevel app mainMenu scene),
                ("exit editor without saving", reallyExitEditor app mainMenu this)
              ])
        ObjectEditMode{} ->
            menu app (Just menuTitle) (Just (edit scene))
            [("exit object edit mode", exitOEM app mainMenu play mvar scene)]
        SelectionMode{} ->
            menu app (Just menuTitle) (Just (edit scene)) [
                ("cut selected objects", edit (cutSelection scene)),
                ("copy selected objects", edit (copySelection scene)),
                ("exit selection mode", edit scene{editorMode = NormalMode})
              ]
  where
    menuTitle = case levelPath scene of
        Nothing -> "editing untitled level"
        Just f -> "editing " ++ f
    edit :: EditorScene Sort_ -> AppState
    edit s = editorLoop app mainMenu play mvar s
    this = editorMenu app mainMenu play mvar scene

    lEnterOEM = case enterOEM app mainMenu play mvar scene of
        Nothing -> []
        Just x -> [("edit object", x)]


saveLevel :: Application -> AppState -> EditorScene Sort_ -> AppState
saveLevel app mainMenu EditorScene{levelPath = (Just path), editorObjects} = AppState $ do
    writeObjectsToDisk path editorObjects
    return mainMenu
saveLevel app mainMenu scene@EditorScene{levelPath = Nothing, editorObjects} = AppState $ do
    name <- askString app "level name:"
    let path = name <|> "nl"
    exists <- doesFileExist path
    if exists then
        return $ fileExists app mainMenu this path editorObjects
      else do
        writeObjectsToDisk path editorObjects
        return mainMenu
  where
    this = saveLevel app mainMenu scene

fileExists app mainMenu save path objects =
    menu app (Just ("level with the name " ++ path ++ " already exists")) (Just save) [
        ("no", save),
        ("yes", writeAnyway)
      ]
  where
    writeAnyway = AppState $ do
        writeObjectsToDisk path objects
        return mainMenu

reallyExitEditor app mainMenu editor =
    menu app (Just "really exit without saving?") (Just editor) [
        ("no", editor),
        ("yes", mainMenu)
      ]

selectSort :: Application -> AppState -> AppState -> PlayLevel -> MVar (EditorScene Sort_)
    -> EditorScene Sort_ -> AppState
selectSort app mainMenu editorMenu play mvar scene =
    treeToMenu app editorMenu (fmap (sortId >>> getSortId) $ availableSorts scene) select
  where
    select :: String -> AppState
    select n =
        editorLoop app mainMenu play mvar scene'
      where
        scene' = case selectFirstElement pred (availableSorts scene) of
            Just newTree -> scene{availableSorts = newTree}
        pred sort = SortId n == sortId sort


enterOEM :: Application -> AppState -> PlayLevel -> MVar (EditorScene Sort_)
    -> EditorScene Sort_ -> Maybe AppState
enterOEM app mainMenu play mvar scene = do -- maybe monad
    i <- selected scene
    _ <- objectEditModeMethods $ editorSort $ getMainObject scene i
    let objects' = modifyMainLayer (modifyByIndex (modifyOEMState mod) i) $ editorObjects scene
        mod :: OEMState Sort_ -> OEMState Sort_
        mod = enterModeOEM scene
    Just $ edit scene{editorMode = ObjectEditMode i, editorObjects = objects'}
  where
    edit :: EditorScene Sort_ -> AppState
    edit s = editorLoop app mainMenu play mvar s

exitOEM :: Application -> AppState -> PlayLevel -> MVar (EditorScene Sort_)
    -> EditorScene Sort_ -> AppState
exitOEM app parent play mvar s =
    editorLoop app parent play mvar s{editorMode = NormalMode}


editLayers :: Application -> AppState -> PlayLevel -> MVar (EditorScene Sort_)
    -> EditorScene Sort_ -> AppState
editLayers app mainMenu play mvar scene =
    menu app (Just "edit layers") (Just editMenu) [
        ("add background layer", edit (addDefaultBackground scene)),
        ("add foreground layer", edit (addDefaultForeground scene))
      ]
  where
    edit s = editorLoop app mainMenu play mvar s
    editMenu = editorMenu app mainMenu play mvar scene


