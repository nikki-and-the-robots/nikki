
-- | benchmarking Base.Animation.pickAnimationFrame


import Utils.Tests

import Base.Animation

import Test.QuickCheck

import Criterion.Main


main = do
    quickCheck $ equalImplementation pickAnimationFrameNaive pickAnimationFrameOld
    quickCheck $ equalImplementation pickAnimationFrameNaive
        (\ a b c -> pickAnimationFrame (mkAnimation a b) c)
    defaultMain (
        bench "pickAnimationFrame" new :
        bench "pickAnimationFrameOld" old :
--         bench "pickAnimationFrameNaive" naive :
        [])

type Seconds = Double
type ImplType = [Char] -> [Seconds] -> Seconds -> Char

equalImplementation :: ImplType -> ImplType -> Property
equalImplementation a b =
    property $
    \ list -> not (null list) ==>
    forAllShrink (listOf positive) shrink $ \ frameTimes ->
    not (null frameTimes) ==>
    forAllShrink positive shrink $ \ now ->
        (a list frameTimes now ?= b list frameTimes now)
  where
    positive :: Gen Double
    positive = suchThat arbitrary (> 0)

new = nf
    (map (pickAnimationFrame (mkAnimation (take 8 ['a' .. 'z']) [0.3, 0.3, 0.2, 1])))
    [0, 0.1 .. 300]

old = nf
    (map (pickAnimationFrameOld (take 8 ['a' .. 'z']) [0.3, 0.3, 0.2, 1]))
    [0, 0.1 .. 300]

naive = nf
    (map (pickAnimationFrameNaive (take 8 ['a' .. 'z']) [0.3, 0.3, 0.2, 1]))
    [0, 0.1 .. 300]
