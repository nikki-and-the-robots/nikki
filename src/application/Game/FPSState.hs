{-# language NamedFieldPuns #-}

-- module to analyse the performance (FPS == Frames Per Second)

module Game.FPSState (
    FpsState,
    initialFPSState,
    tickFPS,
    terminateFpsState,
  ) where


import Prelude hiding ((.))

import Control.Category

import System.IO

import Graphics.Qt

import Base.Configuration as Configuration




-- State for this module (abstract type)
data FpsState
    = FpsState {
        counter :: !Int,
        averageSpan :: !(Maybe Double),
        time :: Maybe (Ptr QTime),
        logHandle :: !Handle
    }
    | NotActivated
  deriving Show




logFile = "fps.dat"

-- creates the initial FpsState
initialFPSState :: IO FpsState
initialFPSState =
    if profiling Configuration.development then do
        logHandle <- openFile logFile WriteMode
        return $ FpsState 0 Nothing Nothing logHandle
      else
        return NotActivated

-- does the actual work. Must be called for every frame
tickFPS :: FpsState -> IO FpsState
tickFPS (FpsState counter avg Nothing logHandle) = do
    -- first time: QTime has to be constructed
    ptr <- newQTime
    startQTime ptr
    return $ FpsState counter avg (Just ptr) logHandle
tickFPS (FpsState counter avg (Just qtime) logHandle) = do
        elapsed <- restartQTime qtime
        log elapsed
        let avg' = calcAvg counter avg elapsed
        handle (FpsState (counter + 1) (Just avg') (Just qtime) logHandle)
  where
    handle x@(FpsState 100 (Just avg) qtime lf) = do
        putStrLn ("(FPS: " ++ show (1000 / avg) ++ ") | ")
--         putStrLn "terminating application for profiling purposes." >> quitQApplication
        return $ FpsState 0 Nothing qtime lf
    handle x = return x

    calcAvg :: Int -> Maybe Double -> QtInt -> Double
    calcAvg 0 Nothing newValue = fromIntegral newValue
    calcAvg len (Just avg) newValue =
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





