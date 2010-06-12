
module Base.Animation where


import Utils

import Control.Arrow

import Base.Constants


-- | should be real-time capable for big nows
pickAnimationFrame :: [a] -> [Seconds] -> Seconds -> a
pickAnimationFrame list frameTimes now =
    inner (drop toDrop $ cycle list) (cycle frameTimes) timeInList
  where
    inner list (frameTime : rFTs) now | now > frameTime =
        inner (tail list) rFTs (now - frameTime)
    inner (a : _) _ _ = a

    -- this is an optimization
    -- timeInList used instead of now
    (n, timeInList) = now `rest` sum frameTimes

    toDrop = (n * length frameTimes) `mod` length list


rest :: Double -> Double -> (Int, Double)
rest a b = properFraction (a / b) |> second (* b)



