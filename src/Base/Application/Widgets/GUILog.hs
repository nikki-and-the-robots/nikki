
module Base.Application.Widgets.GUILog (guiLog, resetGuiLog) where


import Data.Abelian

import Text.Logging

import Control.Monad (join)
import Control.Concurrent
import Control.Concurrent.MVar

import System.IO.Unsafe

import Graphics.Qt

import Utils

import Base.Types
import Base.Font
import Base.Prose
import Base.Constants


-- | Shows log messages in the GUI.
-- Switches the renderer to a functions that shows a log.
-- Adds the given message to the log.
guiLog :: Application_ sort -> Prose -> IO ()
guiLog app msg = do
    logInfo $ p' msg
    mapM_ addMsg $ proseLines msg
    setDrawingCallbackAppWidget (window app) (Just $ renderLog app)

renderLog :: Application_ s -> Ptr QPainter -> IO ()
renderLog app ptr = do
    clearScreen ptr lightBlue
    let font = alphaNumericFont $ applicationPixmaps app
    log <- readMVar logRef
    when (not $ null log) $ do
        size  <- fmap fromIntegral <$> sizeQPainter ptr
        translate ptr (Position (fromUber 3) (height size - fromUber 1))
        forM_ log $ \ line -> do
            translate ptr (Position 0 (- fontHeight font))
            renderLineSimple font white line ptr

-- | global MVar for the log
{-# NOINLINE logRef #-}
logRef :: MVar [Prose]
logRef = unsafePerformIO $ newMVar []

addMsg :: Prose -> IO ()
addMsg msg = modifyMVar_ logRef (return . (msg :))

-- | Empty the gui log queue
resetGuiLog :: IO ()
resetGuiLog = modifyMVar_ logRef (const $ return [])
