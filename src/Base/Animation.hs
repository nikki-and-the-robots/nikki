{-# language DeriveDataTypeable, DeriveFunctor, DeriveFoldable #-}

module Base.Animation (
    Animation,
    mkAnimation,
    isStatic,
    animationHead,
    pickAnimationFrame,
    pickAnimationFrameNaive,
    pickAnimationFrameOld,
    pickAnimationFrameNonLooping,
    pickLimitedAnimationFrame,
  ) where


import Safe

import Data.Data
import Data.Foldable (Foldable)
import Data.List

import Utils


type Seconds = Double


-- * pickAnimationFrame

-- different implementations for testing and optimization

-- | Slow but correct
pickAnimationFrameNaive :: [a] -> [Seconds] -> Seconds -> a
pickAnimationFrameNaive list frameTimes now =
    inner (cycle list) (cycle frameTimes) now
  where
    inner list (frameTime : rFTs) now | now > frameTime =
        inner (tail list) rFTs (now - frameTime)
    inner (a : _) _ _ = a


data Animation a
    = Animation [a] Int [Seconds] Seconds Int
    | Static a
  deriving (Show, Typeable, Data, Foldable)

mkAnimation :: [a] -> [Seconds] -> Animation a
mkAnimation l fs | null l || null fs =
    error "mkAnimation: given frames and frameTimes have to be non-empty."
mkAnimation l _ | length l == 1 =
    let (Just x) = headMay l
    in Static x
mkAnimation l frameTimes =
    Animation l (length l) (mkAbsoluteTimes frameTimes) (sum frameTimes) (length frameTimes)

-- | Returns if the Animation will only ever return one element.
isStatic :: Animation a -> Bool
isStatic Static{} = True
isStatic _ = False

-- | Returns the first element of the Animation.
animationHead :: Animation a -> a
animationHead (Animation (a : r) _ _ _ _) = a
animationHead (Static x) = x

-- | converts a list if timespans to a list of absolute times, starting with (head l).
mkAbsoluteTimes :: [Seconds] -> [Seconds]
mkAbsoluteTimes = inner 0
  where
    inner akk (a : r) =
        let akk' = akk + a
        in akk' : inner akk' r
    inner _ [] = []

-- | should be real-time capable for big nows
-- todo: frameTimes als binärbaum?
pickAnimationFrame :: Animation a -> Seconds -> a
pickAnimationFrame (Static x) _ = x
pickAnimationFrame (Animation list listLen absoluteTimes sum timesLen) now =
    inner (drop toDrop $ cycle list) absoluteTimes timeInList
  where
    inner (a : r) (t : tr) now | now <= t = a
    inner (_ : r) (_ : tr) now = inner r tr now

    -- this is an optimization
    -- timeInList used instead of now
    (n, f) = properFraction (now / sum)
    timeInList = f * sum
    toDrop = (n * timesLen) `mod` listLen


pickAnimationFrameOld :: [a] -> [Seconds] -> Seconds -> a
pickAnimationFrameOld list frameTimes now =
    inner (drop toDrop $ cycle list) (cycle frameTimes) timeInList
  where
    inner list (frameTime : rFTs) now | now >= frameTime =
        inner (tail list) rFTs (now - frameTime)
    inner (a : _) _ _ = a

    -- this is an optimization
    -- timeInList used instead of now
    (n, timeInList) = now `divide` sum frameTimes

    toDrop = (n * length frameTimes) `mod` length list


-- * other animation functions

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
