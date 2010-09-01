{-# language NamedFieldPuns #-}

module Editor.MainLoop (editorLoop, EditorState(..)) where


import Data.Set (Set, empty, toList, insert, delete)

import Control.Concurrent
import Control.Monad.State

import Graphics.Qt

import Utils

import Base.Types
import Base.Events

import Object

import Editor.Scene

import Top.Application
import Top.Pickle


type MM o = StateT EditorState IO o

data EditorState = EditorState {
    scene :: EditorScene Sort_
  }

setScene :: EditorState -> EditorScene Sort_ -> EditorState
setScene _ s = EditorState s


updateSceneMVar :: Application -> MVar (EditorScene Sort_) -> MM ()
updateSceneMVar app mvar = do
    s <- gets scene
    liftIO $ do
        swapMVar mvar s
        updateAppWidget $ window app


-- * menus and states

editorLoop :: Application -> AppState -> MVar (EditorScene Sort_) -> MM AppState
editorLoop app parent sceneMVar = do
    event <- liftIO $ waitForAppEvent $ keyPoller app
    if event == Press StartButton then do
        s <- gets scene
        return $ editorMenu app parent s
      else do
        -- other events are handled below (in Editor.Scene)
        modifies scene setScene (updateEditorScene event)
        updateSceneMVar app sceneMVar
        editorLoop app parent sceneMVar

askSaveLevel :: Application -> AppState -> EditorScene Sort_ -> AppState
askSaveLevel app parent scene@EditorScene{levelPath = (Just path)} =
    Top.Application.menu app (Just ("save level (under name \"" ++ path ++ "\")?")) Nothing [
        ("yes", saveLevel app parent scene),
        ("no", parent)
      ]

saveLevel :: Application -> AppState -> EditorScene Sort_ -> AppState
saveLevel app parent EditorScene{levelPath = (Just path), editorObjects} = AppState $ do
    writeObjectsToDisk path editorObjects
    return parent

editorMenu :: Application -> AppState -> EditorScene Sort_ -> AppState
editorMenu app parent scene =
    Top.Application.menu app Nothing Nothing [
        ("save level and exit editor", saveLevel app parent scene),
        ("exit editor without saving", reallyExitEditor app parent this)
      ]
  where
    this = editorMenu app parent scene

reallyExitEditor app parent editorMenu =
    Top.Application.menu app (Just "really exit without saving?") (Just editorMenu) [
        ("no", editorMenu),
        ("yes", parent)
      ]
