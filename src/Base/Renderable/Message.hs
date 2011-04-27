{-# language ScopedTypeVariables #-}

module Base.Renderable.Message (message) where


import Utils

import Base.Types
import Base.Prose
import Base.Application

import Base.Renderable.Common
import Base.Renderable.WholeScreenPixmap
import Base.Renderable.Layered
import Base.Renderable.Centered
import Base.Renderable.VBox


-- | show a textual message and wait for a keypress
message :: Application_ a -> [Prose] -> AppState -> AppState
message app text follower = appState (MenuBackground |:> centered (vBox text)) $ do
    ignore $ waitForPressButton app
    return follower
