{-# language ScopedTypeVariables, TypeSynonymInstances #-}

-- | Things used in multiple Widgets.

module Base.Renderable.Common where


import System.FilePath

import Graphics.Qt

import Utils

import Base.Constants
import Base.Types
import Base.Polling


backgroundColor :: Color = QtColor 16 0 156 255
standardFontColor :: Color = QtColor 70 210 245 255

menuDir :: FilePath
menuDir = pngDir </> "menu"

-- | Blocks until a Press AppEvent is received.
-- Flushes the event queue before that.
waitForPressAppEvent :: Application_ s -> M Button
waitForPressAppEvent app = do
    ignore $ pollAppEvents app
    inner
  where
    inner = do
        e <- waitForAppEvent app
        case e of
            (Press b) -> return b
            _ -> inner

-- * Renderable

instance Show RenderableInstance where
    show (RenderableInstance x) = "(" ++ show x ++ ")"

instance Renderable RenderableInstance where
    minimalSize app (RenderableInstance r) = minimalSize app r
    render ptr app size (RenderableInstance r) = render ptr app size r


rt :: String -> RenderableInstance
rt = RenderableInstance

-- NYI instance
instance Renderable String where
    minimalSize _ = const $ Size 10 10
    render _ _ _ msg = putStrLn ("NYI: renderable " ++ msg)
