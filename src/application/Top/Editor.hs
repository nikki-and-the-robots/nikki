
-- | contains the application logic to fire up the editor

module Top.Editor where


import Control.Concurrent

import Base.Types

import Object

import Editor.MainLoop


editLevel :: Application -> AppState -> EditorScene Sort_ -> AppState
editLevel app parent s = AppState $ do
    sceneMVar <- newMVar s
    return $ editorLoop app parent sceneMVar s

