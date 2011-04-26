
-- | contains the application logic to fire up the editor

module Top.Editor where


import Control.Concurrent

import Utils

import Base

import Object

import Editor.Menu


editLevel :: Application -> PlayLevel -> EditorScene Sort_ -> AppState
editLevel app play s = NoGUIAppState $ io $ do
    sceneMVar <- newMVar s
    return $ editorLoop app play sceneMVar s

