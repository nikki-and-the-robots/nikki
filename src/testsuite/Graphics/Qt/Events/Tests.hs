
module Graphics.Qt.Events.Tests () where


import Utils

import Graphics.Qt.Events

import Test.QuickCheck


instance Arbitrary Key where
    arbitrary = frequency (
        (1, elements allValues) :
        (10, elements primaryKeys) :
        [])

primaryKeys =
    Ctrl : Shift :
    UpArrow : DownArrow : LeftArrow  : RightArrow :
    Escape : Enter : Return :
    []
