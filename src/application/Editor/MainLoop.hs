{-# language NamedFieldPuns #-}

module Editor.MainLoop (editorLoop) where


import Data.Set (Set, empty, toList, insert, delete)
import Data.Indexable (modifyByIndex)
import Data.SelectTree

import Control.Concurrent
import Control.Monad.State

import Graphics.Qt

import Utils

import Base.Types
import Base.Events
import Base.Grounds

import Object

import Editor.Scene
import Editor.Scene.Types

import Top.Application hiding (selected)
import Top.Pickle


type MM o = StateT (EditorScene Sort_) IO o


updateSceneMVar :: Application -> MVar (EditorScene Sort_) -> MM ()
updateSceneMVar app mvar = do
    s <- get
    liftIO $ do
        swapMVar mvar s
        updateAppWidget $ window app


-- * menus and states

editorLoop :: Application -> AppState -> MVar (EditorScene Sort_)
    -> EditorScene Sort_ -> AppState
editorLoop app mainMenu sceneMVar scene = AppState $ do
    setDrawingCallbackAppWidget (window app) (Just $ render sceneMVar)
    evalStateT worker scene
  where
    worker :: MM AppState
    worker = do
        updateSceneMVar app sceneMVar
        event <- liftIO $ waitForAppEvent $ keyPoller app
        if event == Press StartButton then do
            s <- get
            return $ editorMenu app mainMenu sceneMVar s
          else do
            -- other events are handled below (in Editor.Scene)
            modifyState (updateEditorScene event)
            worker

    render sceneMVar ptr = do
        scene <- readMVar sceneMVar
        renderEditorScene ptr scene



editorMenu :: Application -> AppState -> MVar (EditorScene Sort_)
    -> EditorScene Sort_ -> AppState
editorMenu app mainMenu mvar scene =
    Top.Application.menu app (Just menuTitle) (Just (edit scene))
      (
      lEnterOEM ++
      [
        ("select object", selectSort app mainMenu this mvar scene),
        ("return to editing", edit scene),
        ("edit layers", editLayers app mainMenu mvar scene),
        ("save level and exit editor", saveLevel app mainMenu scene),
        ("exit editor without saving", reallyExitEditor app mainMenu this)
      ])
  where
    menuTitle = case levelPath scene of
        Nothing -> "editing untitled level"
        Just f -> "editing " ++ f
    lEnterOEM = case enterOEM app mainMenu mvar scene of
        Nothing -> []
        Just x -> [x]
    edit :: EditorScene Sort_ -> AppState
    edit s = editorLoop app mainMenu mvar scene
    this = editorMenu app mainMenu mvar scene

saveLevel :: Application -> AppState -> EditorScene Sort_ -> AppState
saveLevel app mainMenu EditorScene{levelPath = (Just path), editorObjects} = AppState $ do
    writeObjectsToDisk path editorObjects
    return mainMenu

reallyExitEditor app mainMenu editor =
    Top.Application.menu app (Just "really exit without saving?") (Just editor) [
        ("no", editor),
        ("yes", mainMenu)
      ]

selectSort :: Application -> AppState -> AppState -> MVar (EditorScene Sort_)
    -> EditorScene Sort_ -> AppState
selectSort app mainMenu editorMenu mvar scene =
    treeToMenu app editorMenu (fmap (sortId >>> getSortId) $ availableSorts scene) select
  where
    select :: String -> AppState
    select n =
        editorLoop app mainMenu mvar scene'
      where
        scene' = case selectFirstElement pred (availableSorts scene) of
            Just newTree -> scene{availableSorts = newTree}
        pred sort = SortId n == sortId sort


enterOEM :: Application -> AppState -> MVar (EditorScene Sort_)
    -> EditorScene Sort_ -> Maybe (String, AppState)
enterOEM app mainMenu mvar scene@EditorScene{editorMode = NormalMode} = do -- maybe monad
    i <- selected scene
    _ <- objectEditModeMethods $ editorSort $ getMainObject scene i
    let objects' = modifyMainLayer (modifyByIndex (modifyOEMState mod) i) $ editorObjects scene
        mod :: OEMState Sort_ -> OEMState Sort_
        mod = enterModeOEM scene
    -- enter oem
    Just $ ("edit object", edit scene{editorMode = ObjectEditMode i, editorObjects = objects'})
  where
    edit :: EditorScene Sort_ -> AppState
    edit s = editorLoop app mainMenu mvar s
enterOEM app parent mvar s@EditorScene{editorMode = ObjectEditMode i} =
    -- exit oem
    Just $ ("exit object edit mode", editorLoop app parent mvar s{editorMode = NormalMode})


editLayers :: Application -> AppState -> MVar (EditorScene Sort_)
    -> EditorScene Sort_ -> AppState
editLayers app mainMenu mvar scene =
    menu app (Just "edit layers") (Just editMenu) [
        ("add background layer", edit (addDefaultBackground scene)),
        ("add foreground layer", edit (addDefaultForeground scene))
      ]
  where
    edit s = editorLoop app mainMenu mvar s
    editMenu = editorMenu app mainMenu mvar scene


