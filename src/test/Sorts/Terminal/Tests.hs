
module Sorts.Terminal.Tests where


import Test.QuickCheck

import Sorts.Terminal


tests =
    mapM_ quickCheck [prop1, prop2] >>
    quickCheck zero >>
    quickCheck full >>
    quickCheck monotone

prop1 numberOfBeams batteryNumberNeeded n =
    (numberOfBeams >= 0) ==>
    roundToBars numberOfBeams batteryNumberNeeded n >= 0

prop2 numberOfBeams batteryNumberNeeded n =
    (numberOfBeams >= 0) ==>
    roundToBars numberOfBeams batteryNumberNeeded n >= 0

zero numberOfBeams batteryNumberNeeded =
    (batteryNumberNeeded > 0) ==>
    roundToBars numberOfBeams batteryNumberNeeded 0 == 0

full numberOfBeams batteryNumberNeeded n =
    (n >= batteryNumberNeeded) ==>
    roundToBars numberOfBeams batteryNumberNeeded n == numberOfBeams

monotone numberOfBeams batteryNumberNeeded a b =
    (b > a && numberOfBeams >= 0) ==>
    f b >= f a
  where
    f = roundToBars numberOfBeams batteryNumberNeeded
