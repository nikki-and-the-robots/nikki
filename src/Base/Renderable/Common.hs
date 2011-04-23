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
standardFontColor :: Color = QtColor 115 115 255 255

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

rt :: String -> RenderableInstance
rt = RenderableInstance

-- NYI instance
instance Renderable String where
    render _ _ msg = (Size 10 10, const $ putStrLn ("NYI: renderable " ++ msg))

instance Renderable RenderableInstance where
    render app size (RenderableInstance r) = render app size r
