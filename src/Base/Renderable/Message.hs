{-# language ScopedTypeVariables #-}

module Base.Renderable.Message (message) where


import Utils

import Base.Types
import Base.Prose
import Base.Application
import Base.Monad
import Base.Configuration
import Base.Configuration.Controls

import Base.Renderable.Common
import Base.Renderable.WholeScreenPixmap
import Base.Renderable.Layered
import Base.Renderable.Centered
import Base.Renderable.VBox
import Base.Renderable.StickToBottom


-- | show a textual message and wait for a keypress
message :: Application -> [Prose] -> AppState -> AppState
message app text follower = appState renderable $ do
    controls__ <- controls_ <$> getConfiguration
    ignore $ waitForPressedButton app
    return follower
  where
    renderable = MenuBackground |:>
        addKeysHint PressAnyKey (centered (vBox 1 text))
