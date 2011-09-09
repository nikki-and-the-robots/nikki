
-- module to analyse the performance (FPS == Frames Per Second)

module Profiling.FPS (
    FpsState,
    FPSRef,
    initialFPSState,
    tickFPS,
    initialFPSRef,
    tickFPSRef,
  ) where


import Prelude hiding ((.))

import Data.IORef
import Data.Time.Clock.POSIX

import Text.Printf

import Control.Category

import Graphics.Qt

import Utils

import Base.Constants
import Base.Configuration as Configuration
import Base.Types
import Base.Monad
import Base.Prose
import Base.Renderable ()


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
tickFPS :: Application -> Configuration -> Ptr QPainter -> FpsState -> IO FpsState
tickFPS _ config ptr (FpsState counter avg Nothing displayValue) = do
    -- first time: QTime has to be constructed
    now <- getNow
    return $ FpsState counter avg (Just now) displayValue
tickFPS app config ptr (FpsState counter avg (Just oldTime) displayValue) = do
        now <- getNow
        let elapsed = now - oldTime
            avg' = calcAvg counter avg elapsed
        r <- handle (FpsState (counter + 1) (Just avg') (Just now) displayValue)
        io (renderFPS app config ptr =<< readIORef displayValue)
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
tickFPS _ _ _ x = return x

renderFPS :: Application -> Configuration -> Ptr QPainter -> Prose -> IO ()
renderFPS app config ptr fps = do
    resetMatrix ptr
    size <- sizeQPainter ptr
    (renderSize, action) <- render ptr app config size (False, fps)
    translate ptr $ Position (fromUber 1) (height size - height renderSize)
    action

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

tickFPSRef :: Application -> Configuration -> Ptr QPainter -> FPSRef -> IO ()
tickFPSRef app config ptr (FPSRef ref) =
    readIORef ref >>= tickFPS app config ptr >>= writeIORef ref
