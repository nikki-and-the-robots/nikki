{-# language NamedFieldPuns #-}

-- module to analyse the performance (FPS == Frames Per Second)

module Base.FPSState (
    FpsState,
    initialFPSState,
    tickFPS,
    terminateFpsState,
    initialFPSRef,
    tickFPSRef,
    terminateFPSRef,
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
import Base.Types
import Base.Monad


-- | returns the seconds since epoch start
getNow = realToFrac <$> getPOSIXTime

-- State for this module (abstract type)
data FpsState
    = FpsState {
        counter :: !Int,
        averageSpan :: !(Maybe Double),
        oldTime :: Maybe Double,
        logHandle :: Maybe Handle,
        displayValue :: IORef String
    }
    | NotActivated


logFile = "fps.dat"

-- creates the initial FpsState
initialFPSState :: M FpsState
initialFPSState = do
    graphicsProfiling_ <- gets graphics_profiling
    if graphicsProfiling_ then do
--         logHandle <- openFile logFile WriteMode
--         return $ FpsState 0 Nothing Nothing logHandle
        displayValue <- io $ newIORef ""
        return $ FpsState 0 Nothing Nothing Nothing displayValue
      else
        return NotActivated

-- does the actual work. Must be called for every frame
tickFPS :: Ptr QPainter -> FpsState -> IO FpsState
tickFPS ptr (FpsState counter avg Nothing logHandle displayValue) = do
    -- first time: QTime has to be constructed
    now <- getNow
    return $ FpsState counter avg (Just now) logHandle displayValue
tickFPS ptr (FpsState counter avg (Just oldTime) logHandle displayValue) = do
        now <- getNow
        let elapsed = now - oldTime
        log elapsed
        let avg' = calcAvg counter avg elapsed
        r <- handle (FpsState (counter + 1) (Just avg') (Just now) logHandle displayValue)
        io (renderFPS ptr =<< readIORef displayValue)
        return r
  where
    handle x@(FpsState 10 (Just avg) qtime lf dv) = do
        writeIORef dv (printf "FPS: %3.1f" (1 / avg))
        return $ FpsState 0 Nothing qtime lf dv
    handle x = return x

    calcAvg :: Int -> Maybe Double -> Double -> Double
    calcAvg 0 Nothing newValue = newValue
    calcAvg len (Just avg) newValue =
        (lenF * avg + newValue) / (lenF + 1)
      where lenF = fromIntegral len

    log elapsed = whenMaybe logHandle $ \ h -> 
                    hPutStrLn h (show elapsed)
-- no FPS activated (NotActivated)
tickFPS _ x = return x

renderFPS :: Ptr QPainter -> String -> IO ()
renderFPS ptr fps = do
    resetMatrix ptr
    drawText ptr (Position 30 30) False fps

terminateFpsState :: FpsState -> IO ()
terminateFpsState FpsState{logHandle = Just h} = do
    hClose h
    writeDistribution logFile "profiling/distribution.dat"
-- NotActivated
terminateFpsState _ = return ()


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

tickFPSRef :: Ptr QPainter -> FPSRef -> IO ()
tickFPSRef ptr (FPSRef ref) =
    readIORef ref >>= tickFPS ptr >>= writeIORef ref

terminateFPSRef :: FPSRef -> IO ()
terminateFPSRef (FPSRef ref)  =
    readIORef ref >>= terminateFpsState
