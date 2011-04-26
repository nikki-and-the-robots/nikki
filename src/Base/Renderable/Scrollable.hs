{-# language ScopedTypeVariables #-}

module Base.Renderable.Scrollable (
    scrollingAppState
  ) where


import Data.Abelian
import Data.IORef
import Data.List

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


scrollingAppState :: Application_ s -> [Prose] -> AppState -> AppState
scrollingAppState app text follower = ioAppState (rt "scrollingAppState") $ do
    (renderable, sendCommand) <- scrollable app text
    return $ AppState renderable $ loop sendCommand
  where
    loop :: ((Int -> Int) -> IO ()) -> M AppState
    loop send = do
        e <- waitForPressButton app
        if isDown e then
            io (send (+ 1)) >>
            loop send
          else if isUp e then
            io (send (subtract 1)) >>
            loop send
          else
            return follower

scrollable :: Application_ s -> [Prose] -> IO (RenderableInstance, (Int -> Int) -> IO ())
scrollable app lines = do
    chan <- newChan
    scrollDownRef <- newIORef 0
    let r = RenderableInstance (
            MenuBackground |:>
            (centerHorizontally $ Scrollable (wordWrap app textWidth lines) chan scrollDownRef)
          )
        send fun = do
            writeChan chan fun
            updateGLContext (window app)
    return (r, send)

data Scrollable = Scrollable [[Glyph]] (Chan (Int -> Int)) (IORef Int)

instance Show Scrollable where
    show = const "<Scrollable>"

textWidth :: Double = 800

instance Renderable Scrollable where
    render ptr app parentSize (Scrollable lines chan scrollDownRef) = do
        let h = height parentSize
            widgetSize = Size textWidth h
        lineRenders <- fmapM (render ptr app widgetSize) lines
        scrollDown <- updateScrollDown (maximalScrollDown h lineRenders) chan scrollDownRef
        let action = forM_ (clipHeight h $ drop scrollDown lineRenders) $
                \ (itemSize, itemAction) -> do
                    recoverMatrix ptr $ itemAction
                    translate ptr (Position 0 (height itemSize))
        return (widgetSize, action)

-- | Updates the scrollDown according to the widget size and events.
-- Returns the current scrollDown.
updateScrollDown :: Int -> Chan (Int -> Int) -> IORef Int -> IO Int
updateScrollDown maximalScrollDown chan ref = do
    events <- pollChannel chan
    modifyIORef ref (min maximalScrollDown . max 0 . foldr (.) id events)
    readIORef ref

-- | Returns the maximal scrollDown for a given height and child sizes (and actions).
maximalScrollDown :: Double -> [(Size Double, IO ())] -> Int
maximalScrollDown h [] = 0
maximalScrollDown h widgets =
    length widgets - numberOfItemsWhenScrolledDown
  where
    numberOfItemsWhenScrolledDown = length $ takeWhile (< h) summedHeights
    summedHeights = fmap sum $ tail $ inits heights
    heights = fmap (height . fst) $ reverse widgets

-- | Removes the widgets at the end of the list that don't fit.
clipHeight :: Double -> [(Size Double, IO ())] -> [(Size Double, IO ())]
clipHeight h [] = []
clipHeight h (a : r) =
    if itemHeight > h then [] else a : clipHeight (h - itemHeight) r
  where
    itemHeight = height $ fst a
