{-# language ScopedTypeVariables #-}

module Base.Renderable.Scrollable (
    scrollingAppState
  ) where


import Data.IORef
import Data.List

import Control.Concurrent

import Graphics.Qt

import Utils

import Base.Prose
import Base.Types
import Base.Font
import Base.Monad

import Base.Configuration
import Base.Configuration.Controls

import Base.Renderable.Common
import Base.Renderable.WholeScreenPixmap
import Base.Renderable.Layered
import Base.Renderable.Centered
import Base.Renderable.Spacer


textWidth = 800

-- | Shows a text till some key is pressed.
-- Adds an empty line at the top.
scrollingAppState :: Application_ s -> [Prose] -> AppState -> AppState
scrollingAppState app text follower = NoGUIAppState $ io $ do
    (renderable, sendCommand) <- scrollable app text
    return $ AppState renderable $ loop sendCommand
  where
    loop :: ((Int -> Int) -> IO ()) -> M AppState
    loop send = do
        e <- waitForPressButton app
        controls_ <- gets controls
        if isMenuDown controls_ e then
            io (send (+ 1)) >>
            loop send
          else if isMenuUp controls_ e then
            io (send (subtract 1)) >>
            loop send
          else
            return follower

scrollable :: Application_ s -> [Prose] -> IO (RenderableInstance, (Int -> Int) -> IO ())
scrollable app children = do
    chan <- newChan
    scrollDownRef <- newIORef 0
    let r = MenuBackground |:>
            (centered $
            parentSpacer (\ (Size w h) -> Size textWidth h) $
            Scrollable children chan scrollDownRef)
        send fun = do
            writeChan chan fun
            updateGLContext (window app)
    return (renderable r, send)

data Scrollable = Scrollable [Prose] (Chan (Int -> Int)) (IORef Int)

instance Show Scrollable where
    show = const "<Scrollable>"

instance Renderable Scrollable where
    render ptr app config parentSize (Scrollable children chan scrollDownRef) = do
        let h = height parentSize
            widgetSize = parentSize
            lines = concatMap (wordWrap app (width parentSize)) children
        lineRenders <-
            fmapM (render ptr app config widgetSize) $
            addSpacer $ fmap spacerForNull $
            lines
        scrollDown <- updateScrollDown (maximalScrollDown h lineRenders) chan scrollDownRef
        let action = forM_ (clipHeight h $ drop scrollDown lineRenders) $
                \ (itemSize, itemAction) -> do
                    recoverMatrix ptr $ itemAction
                    translate ptr (Position 0 (height itemSize))
        return (widgetSize, action)
      where
        lineSpacer = renderable $ emptySpacer (const $ Size 0 fontHeight)
        addSpacer = (lineSpacer :)
        spacerForNull :: [Glyph] -> RenderableInstance
        spacerForNull [] = lineSpacer
        spacerForNull x = renderable x

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
