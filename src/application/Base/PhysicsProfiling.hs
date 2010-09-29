{-# language ViewPatterns, ScopedTypeVariables #-}
-- | allows to show how many busy wait cycles are used in the physics thread

module Base.PhysicsProfiling (tickBusyWaitCounter) where


import Data.IORef
import Data.Initial

import System.IO.Unsafe

import Base.Configuration


-- | tell the profiler how many busy wait cycles were used
tickBusyWaitCounter :: Int -> IO ()
tickBusyWaitCounter _ | not (profiling development) = return ()
tickBusyWaitCounter (fromIntegral -> waited :: Double) = do
    accu <- readIORef busyWaitCounter
    let zero = if waited == 0 then 1 else 0
        accu' = case accu of
            AverageAccumulator sum n zeros ->
                AverageAccumulator (sum + waited) (n + 1) (zeros + zero)
            New -> AverageAccumulator waited 1 zero
    accu'' <- if counter accu' >= counterLimit then do
        putStrLn $ summary accu'
        return initial
      else
        return accu'
    writeIORef busyWaitCounter accu''

-- | after so many ticks, the average will be printed
counterLimit = 1000

{-# NOINLINE busyWaitCounter #-}
busyWaitCounter :: IORef AverageAccumulator
busyWaitCounter = unsafePerformIO $ newIORef initial

-- | can store a sum and the number of summands
data AverageAccumulator
    = New
    | AverageAccumulator {
        sum :: Double,
        counter :: Int,
        zeros :: Int
      }

instance Initial AverageAccumulator where
    initial = AverageAccumulator 0 0 0

-- | calculates the accumulated average
average :: AverageAccumulator -> Double
average (AverageAccumulator sum counter _) = sum / fromIntegral counter

-- | provides a summary of the AverageAccumulator
summary :: AverageAccumulator -> String
summary a =
    "Average numbers of busy wait loops: " ++ show (average a)
    ++ " number of no wait cycles: " ++ show (zeros a)

