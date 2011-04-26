{-# language ScopedTypeVariables #-}

module Base.Renderable.Scrollable (
    scrollingAppState
  ) where


import Data.Abelian
import Data.IORef

import Control.Concurrent

import Graphics.Qt

import Utils

import Base.Prose
import Base.Types
import Base.Font
import Base.Application

import Base.Renderable.Common
import Base.Renderable.VBox
import Base.Renderable.WholeScreenPixmap
import Base.Renderable.Layered
import Base.Renderable.Centered
import Base.Renderable.CenterHorizontally


textWidth :: Double = 800

scrollingAppState :: Application_ s -> [Prose] -> AppState -> AppState
scrollingAppState app text follower = ioAppState (rt "scrollingAppState") $ do
    stateRef <- mkStateRef $ wordWrap app textWidth text
    return $ appState (scrollable Nothing stateRef) $ loop stateRef
  where
    loop ref = do
        e <- waitForPressButton app
        if isScrollableButton e then do
            return $ appState (scrollable (Just e) ref) $ loop ref
          else
            return follower

mkStateRef lines = newIORef (State lines 0)

scrollable :: Maybe Button -> IORef State -> RenderableInstance
scrollable mButton ref =
    RenderableInstance (
        MenuBackground |:>
        (centerHorizontally $ Scrollable mButton ref)
      )

isScrollableButton :: Button -> Bool
isScrollableButton x = isDown x || isUp x

data Scrollable = Scrollable (Maybe Button) (IORef State)

data State = State [[Glyph]] Int
                          -- scrolldown

instance Renderable Scrollable where
    render ptr app parentSize (Scrollable mButton ref) =
        (Size textWidth 30, action)
      where
        action = do
            newState <- updateState mButton <$> readIORef ref
            writeIORef ref newState
            let (State lines scrollDown) = newState
            snd $ render ptr app (Size textWidth (height parentSize)) $
                vBox (drop scrollDown lines)

-- | applies the scrolling events to the scrollDown
updateState :: (Maybe Button) -> State -> State
updateState Nothing state = state
updateState (Just button) s@(State lines scrollDown) =
         if isDown button then State lines (scrollDown + 1)
    else if isUp   button then State lines (scrollDown - 1)
    else s
