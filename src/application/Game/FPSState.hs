{-# language NamedFieldPuns #-}

-- module to analyse the performance (FPS == Frames Per Second)

module Game.FPSState (
    FpsState,
    initialFPSState,
    tickFPS,
    terminateFpsState,
  ) where


import Prelude hiding ((.))
import Utils
import qualified Strict

import Control.Category

import System.IO

import Graphics.Qt

import Configuration




-- State for this module (abstract type)
data FpsState
    = FpsState {
        counter :: !Int,
        averageSpan :: !(Strict.Maybe Double),
        time :: Strict.Maybe (Ptr QTime),
        logHandle :: !Handle
    }
    | NotActivated
  deriving Show




logFile = "profiling/fps.dat"

-- creates the initial FpsState
initialFPSState :: IO FpsState
initialFPSState =
    if profiling Configuration.development then do
        logHandle <- openFile logFile WriteMode
        return $ FpsState 0 Strict.Nothing Strict.Nothing logHandle
      else
        return NotActivated

-- does the actual work. Must be called for every frame
tickFPS :: FpsState -> IO FpsState
tickFPS (FpsState counter avg Strict.Nothing logHandle) = do
    -- first time: QTime has to be constructed
    ptr <- newQTime
    startQTime ptr
    return $ FpsState counter avg (Strict.Just ptr) logHandle
tickFPS (FpsState counter avg (Strict.Just qtime) logHandle) = do
        elapsed <- restartQTime qtime
        log elapsed
        let avg' = calcAvg counter avg elapsed
        handle (FpsState (counter + 1) (Strict.Just avg') (Strict.Just qtime) logHandle)
  where
    handle x@(FpsState 100 (Strict.Just avg) qtime lf) = do
        putStrLn ("(FPS: " ++ show (1000 / avg) ++ ") | ")
--         putStrLn "terminating application for profiling purposes." >> quitQApplication
        return $ FpsState 0 Strict.Nothing qtime lf
    handle x = return x

    calcAvg :: Int -> Strict.Maybe Double -> QtInt -> Double
    calcAvg 0 Strict.Nothing newValue = fromIntegral newValue
    calcAvg len (Strict.Just avg) newValue =
        (lenF * avg + fromIntegral newValue) / (lenF + 1)
      where lenF = fromIntegral len

    log elapsed = hPutStrLn logHandle (show elapsed)
-- no FPS activated (NotActivated)
tickFPS x = return x

terminateFpsState :: FpsState -> IO ()
terminateFpsState FpsState{logHandle} = do
    hClose logHandle
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





