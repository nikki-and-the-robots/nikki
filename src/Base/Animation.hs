
module Base.Animation where


import Utils

import Base.Types


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
    (n, timeInList) = now `divide` sum frameTimes

    toDrop = (n * length frameTimes) `mod` length list

-- | picks animation frames from infinite lists
-- has O(now) :( todo
pickAnimationFrameNonLooping :: [a] -> [(Int, Seconds)] -> Seconds -> a
pickAnimationFrameNonLooping list ((i, secs) : r) now =
    if now >= secs then
        pickAnimationFrameNonLooping list r (now - secs)
      else
        cycle list !! i

-- | picks an animation frame from an animation that ends. Will return Nothing after animation ends
pickLimitedAnimationFrame :: [a] -> [(Int, Seconds)] -> Seconds -> Maybe a
pickLimitedAnimationFrame list ((i, secs) : r) now =
    if now >= secs then
        pickLimitedAnimationFrame list r (now - secs)
      else
        Just (cycle list !! i)
pickLimitedAnimationFrame _ [] _ = Nothing
