
-- | contains the application logic to fire up the editor

module Top.Editor where


import Control.Concurrent

import Base.Types
import Base.Application

import Object

import Editor.Menu


editLevel :: Application -> PlayLevel -> EditorScene Sort_ -> AppState
editLevel app play s = ioAppState $ do
    sceneMVar <- newMVar s
    return $ editorLoop app play sceneMVar s

