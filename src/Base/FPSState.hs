{-# language NamedFieldPuns #-}

-- module to analyse the performance (FPS == Frames Per Second)

module Base.FPSState (
    FpsState,
    initialFPSState,
    tickFPS,
    initialFPSRef,
    tickFPSRef,
  ) where


import Prelude hiding ((.))

import Data.IORef
import Data.Time.Clock.POSIX

import Text.Logging
import Text.Printf

import Control.Category

import System.IO

import Graphics.Qt

import Utils

import Base.Configuration as Configuration
import Base.Constants
import Base.Types
import Base.Monad
import Base.Prose
import Base.Font


-- | returns the seconds since epoch start
getNow = realToFrac <$> getPOSIXTime

-- State for this module (abstract type)
data FpsState
    = FpsState {
        counter :: !Int,
        averageSpan :: !(Maybe Double),
        oldTime :: Maybe Double,
        displayValue :: IORef Prose
    }
    | NotActivated


-- creates the initial FpsState
initialFPSState :: M FpsState
initialFPSState = do
    graphicsProfiling_ <- gets graphics_profiling
    if graphicsProfiling_ then do
        displayValue <- io $ newIORef (pVerbatim "")
        return $ FpsState 0 Nothing Nothing displayValue
      else
        return NotActivated

-- does the actual work. Must be called for every frame
tickFPS :: Font -> Ptr QPainter -> FpsState -> IO FpsState
tickFPS font ptr (FpsState counter avg Nothing displayValue) = do
    -- first time: QTime has to be constructed
    now <- getNow
    return $ FpsState counter avg (Just now) displayValue
tickFPS font ptr (FpsState counter avg (Just oldTime) displayValue) = do
        now <- getNow
        let elapsed = now - oldTime
            avg' = calcAvg counter avg elapsed
        r <- handle (FpsState (counter + 1) (Just avg') (Just now) displayValue)
        io (renderFPS font ptr =<< readIORef displayValue)
        return r
  where
    handle x@(FpsState 10 (Just avg) qtime dv) = do
        writeIORef dv (pVerbatim $ printf "FPS: %3.1f" (1 / avg))
        return $ FpsState 0 Nothing qtime dv
    handle x = return x

    calcAvg :: Int -> Maybe Double -> Double -> Double
    calcAvg 0 Nothing newValue = newValue
    calcAvg len (Just avg) newValue =
        (lenF * avg + newValue) / (lenF + 1)
      where lenF = fromIntegral len

-- no FPS activated (NotActivated)
tickFPS _ _ x = return x

renderFPS :: Font -> Ptr QPainter -> Prose -> IO ()
renderFPS font ptr fps = do
    resetMatrix ptr
    size <- fmap fromIntegral <$> sizeQPainter ptr
    translate ptr (Position (fromUber 2) (height size - fontHeight font))
    void $ renderLineSimple font Nothing white fps ptr

writeDistribution :: FilePath -> FilePath -> IO ()
writeDistribution srcFile destFile = do
    readFile srcFile >>= (writeFile destFile . inner)
  where
    inner :: String -> String
    inner =
        lines >>> map read >>>
        calculateDistribution >>>
        map (\ (a, b) -> show a ++ " " ++ show b) >>> unlines

calculateDistribution :: [Int] -> [(Int, Int)]
calculateDistribution list =
    map (\ n -> (n, length (filter (== n) list))) [minimum list .. maximum list]


-- * FPSRef

newtype FPSRef = FPSRef (IORef FpsState)

initialFPSRef :: M FPSRef
initialFPSRef =
    initialFPSState >>= io . newIORef >>= return . FPSRef

tickFPSRef :: Font -> Ptr QPainter -> FPSRef -> IO ()
tickFPSRef font ptr (FPSRef ref) =
    readIORef ref >>= tickFPS font ptr >>= writeIORef ref
