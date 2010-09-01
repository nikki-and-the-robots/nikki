
-- | contains the application logic to fire up the editor

module Top.Editor where


import Data.Set (Set, empty, toList, insert, delete)

import Control.Concurrent
import Control.Monad.State

import Graphics.Qt

import Utils

import Base.Types

import Object

import Editor.MainLoop
import Editor.Scene

import Top.Application
import Top.Pickle


editLevel :: Application -> AppState -> EditorScene Sort_ -> AppState
editLevel app parent s = AppState $ do
    sceneMVar <- newMVar s
    setDrawingCallbackAppWidget (window app) (Just $ render sceneMVar)
    evalStateT (editorLoop app parent sceneMVar) (EditorState empty s)
  where
    render sceneMVar ptr = do
        scene <- readMVar sceneMVar
        renderEditorScene ptr scene

