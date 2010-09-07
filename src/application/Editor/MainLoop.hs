{-# language NamedFieldPuns #-}

module Editor.MainLoop (PlayLevel, editorLoop) where


import Data.Indexable (modifyByIndex)
import Data.SelectTree

import Control.Concurrent
import Control.Monad.State

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
editorLoop app mainMenu play sceneMVar scene = AppState $ do
    setDrawingCallbackAppWidget (window app) (Just $ render sceneMVar)
    evalStateT worker scene
  where
    worker :: MM AppState
    worker = do
        updateSceneMVar app sceneMVar
        event <- liftIO $ waitForAppEvent $ keyPoller app
        if event == Press StartButton then do
            s <- get
            return $ editorMenu app mainMenu play sceneMVar s
          else do
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


