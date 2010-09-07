
-- | contains the application logic to fire up the editor

module Top.Editor where


import Control.Concurrent

import Base.Types
import Base.Application

import Object

import Editor.MainLoop


editLevel :: Application -> AppState -> PlayLevel -> EditorScene Sort_ -> AppState
editLevel app parent play s = AppState $ do
    sceneMVar <- newMVar s
    return $ editorLoop app parent play sceneMVar s

