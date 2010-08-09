
-- | contains the application logic to fire up the editor

module Top.Editor where


import Data.Set (Set, empty, toList, insert, delete)

import Control.Concurrent
import Control.Monad.State

import Graphics.Qt

import Utils

import Base.Types

import Object

import Editor.Scene

import Top.Application
import Top.Pickle


editLevel :: Application -> AppState -> EditorScene Sort_ -> AppState
editLevel app parent s = AppState $ do
    sceneMVar <- newMVar s
    setDrawingCallbackAppWidget (window app) (Just $ render sceneMVar)

    evalStateT (loop sceneMVar) (EditorState empty s)
  where
    render sceneMVar ptr = do
        scene <- readMVar sceneMVar
        renderEditorScene ptr scene

    loop ::MVar (EditorScene Sort_) -> MM AppState
    loop sceneMVar = do
        event <- liftIO $ readNextEvent $ keyPoller app
        heldKeys <- updateKeyState event
        modifies scene setScene (updateEditorScene (ControlData [event] heldKeys))
        s <- gets scene
        case s of
            ExitEditorScene{} ->
                return $ askSaveLevel app parent s
            _ -> do
                updateSceneMVar app sceneMVar
                loop sceneMVar

askSaveLevel :: Application -> AppState -> EditorScene Sort_ -> AppState
askSaveLevel app parent scene@(ExitEditorScene (Just path) _) =
    Top.Application.menu app (Just ("save level (under name \"" ++ path ++ "\")?")) Nothing [
        ("yes", saveLevel app parent scene),
        ("no", parent)
      ]

saveLevel :: Application -> AppState -> EditorScene Sort_ -> AppState
saveLevel app parent (ExitEditorScene (Just path) grounds) = AppState $ do
    writeObjectsToDisk path grounds
    return parent



type MM o = StateT EditorState IO o

data EditorState = EditorState {
    keyState :: Set Key,
    scene :: EditorScene Sort_
  }

setKeyState :: EditorState -> Set Key -> EditorState
setKeyState (EditorState _ b) a = EditorState a b
setScene :: EditorState -> EditorScene Sort_ -> EditorState
setScene    (EditorState a _ ) b = EditorState a b

updateKeyState :: QtEvent -> MM [Key]
updateKeyState event = do
    modifies keyState setKeyState (inner event)
    toList <$> gets keyState
  where
    inner :: QtEvent -> Set Key -> Set Key
    inner (KeyPress k) ll = insert k ll
    inner (KeyRelease k) ll = delete k ll

updateSceneMVar :: Application -> MVar (EditorScene Sort_) -> MM ()
updateSceneMVar app mvar = do
    s <- gets scene
    liftIO $ do
        swapMVar mvar s
        updateAppWidget $ window app

