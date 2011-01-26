
module Base.Application.GUILog (guiLog) where


import Data.Abelian

import Text.Logging

import Control.Concurrent.MVar

import System.IO.Unsafe

import Graphics.Qt

import Utils

import Base.Types


-- | Shows log messages in the GUI.
-- Switches the renderer to a functions that shows a log.
-- Adds the given message to the log.
guiLog :: Application_ sort -> String -> IO ()
guiLog app msg = do
    logInfo msg
    mapM_ addMsg $ lines msg
    setDrawingCallbackAppWidget (window app) (Just renderLog)

renderLog :: Ptr QPainter -> IO ()
renderLog ptr = do
    clearScreen ptr lightBlue
    log <- readMVar logRef
    when (not $ null log) $ do
        size  <- fmap fromIntegral <$> sizeQPainter ptr
        translate ptr (Position 30 (height size))
        forM_ log $ \ line -> do
            translate ptr (Position 0 (- 30))
            drawText ptr zero False line

-- | global MVar for the log
{-# NOINLINE logRef #-}
logRef :: MVar [String]
logRef = unsafePerformIO $ newMVar []

addMsg :: String -> IO ()
addMsg msg = modifyMVar_ logRef (return . (msg :))
