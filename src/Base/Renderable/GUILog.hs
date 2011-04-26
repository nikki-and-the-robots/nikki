
module Base.Renderable.GUILog (guiLog, resetGuiLog) where


import Text.Logging

import Control.Concurrent

import System.IO.Unsafe

import Graphics.Qt

import Utils

import Base.Types
import Base.Font
import Base.Prose
import Base.Constants

import Base.Renderable.Common


-- | Shows log messages in the GUI.
-- Switches the renderer to a functions that shows a log.
-- Adds the given message to the log.
guiLog :: Application_ sort -> Prose -> IO ()
guiLog app msg = do
    logInfo $ unP msg
    mapM_ addMsg $ proseLines msg
    setDrawingCallbackGLContext (window app) (Just $ renderLog app)

renderLog :: Application_ s -> Ptr QPainter -> IO ()
renderLog app ptr = do
    clearScreen ptr backgroundColor
    let font = alphaNumericFont $ applicationPixmaps app
    log <- readMVar logRef
    when (not $ null log) $ do
        size <- fmap fromIntegral <$> sizeQPainter ptr
        translate ptr (Position (fromUber 3) (height size - fromUber 1))
        forM_ log $ \ line -> do
            translate ptr (Position 0 (- fontHeight font))
            todo
--             renderLineSimple font Nothing standardFontColor line ptr

-- | global MVar for the log
{-# NOINLINE logRef #-}
logRef :: MVar [Prose]
logRef = unsafePerformIO $ newMVar []

addMsg :: Prose -> IO ()
addMsg msg = modifyMVar_ logRef (return . (msg :))

-- | Empty the gui log queue
resetGuiLog :: IO ()
resetGuiLog = modifyMVar_ logRef (const $ return [])
