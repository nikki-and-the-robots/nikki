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

import Control.Category

import System.IO

import Graphics.Qt

import Utils

import Base.Configuration as Configuration
import Base.Timer




-- State for this module (abstract type)
data FpsState
    = FpsState {
        counter :: !Int,
        averageSpan :: !(Maybe Double),
        oldTime :: Maybe Double,
        logHandle :: Maybe Handle
    }
    | NotActivated
  deriving Show


logFile = "fps.dat"

-- creates the initial FpsState
initialFPSState :: IO FpsState
initialFPSState =
    if graphicsProfiling Configuration.development then do
--         logHandle <- openFile logFile WriteMode
--         return $ FpsState 0 Nothing Nothing logHandle
        return $ FpsState 0 Nothing Nothing Nothing
      else
        return NotActivated

-- does the actual work. Must be called for every frame
tickFPS :: FpsState -> IO FpsState
tickFPS (FpsState counter avg Nothing logHandle) = do
    -- first time: QTime has to be constructed
    now <- getNow
    return $ FpsState counter avg (Just now) logHandle
tickFPS (FpsState counter avg (Just oldTime) logHandle) = do
        now <- getNow
        let elapsed = now - oldTime
        log elapsed
        let avg' = calcAvg counter avg elapsed
        handle (FpsState (counter + 1) (Just avg') (Just now) logHandle)
  where
    handle x@(FpsState 10 (Just avg) qtime lf) = do
        putStrLn ("(FPS: " ++ show (1 / avg) ++ ") | ")
--         putStrLn "terminating application for profiling purposes." >> quitQApplication
        return $ FpsState 0 Nothing qtime lf
    handle x = return x

    calcAvg :: Int -> Maybe Double -> Double -> Double
    calcAvg 0 Nothing newValue = newValue
    calcAvg len (Just avg) newValue =
        (lenF * avg + newValue) / (lenF + 1)
      where lenF = fromIntegral len

    log elapsed = whenMaybe logHandle $ \ h -> 
                    hPutStrLn h (show elapsed)
-- no FPS activated (NotActivated)
tickFPS x = return x

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

initialFPSRef :: IO FPSRef
initialFPSRef =
    initialFPSState >>= newIORef >>= return . FPSRef

tickFPSRef :: FPSRef -> IO ()
tickFPSRef (FPSRef ref) =
    readIORef ref >>= tickFPS >>= writeIORef ref

terminateFPSRef :: FPSRef -> IO ()
terminateFPSRef (FPSRef ref)  =
    readIORef ref >>= terminateFpsState
