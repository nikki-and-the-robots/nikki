{-# language ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances, ViewPatterns,
    DeriveDataTypeable, StandaloneDeriving #-}

module Physics.Chipmunk.StickyEdges.Tests where


import Prelude hiding (catch)

import Data.Initial
import qualified Data.Indexable as I
import Data.Abelian
import Data.Typeable
import Data.List

import Control.Monad
import Control.Applicative ((<$>), (<**>))
import Control.Concurrent
import Control.Exception

import System.Random

import Test.QuickCheck
import Test.QuickCheck.Gen

import Graphics.Qt hiding (scale)

import Physics.Chipmunk
import Physics.Chipmunk.StickyEdges

import Utils

import Base.Grounds
import Base.Types

import Object

import Editor.Pickle

import Physics.Chipmunk.StickyEdges.Tests.Properties
import Physics.Chipmunk.StickyEdges.Tests.Rendering


-- | composition of all tests in this module
tests :: IO ()
tests = do
    testPred "stickyEdgesRemovable" stickyEdgesRemovable
    testPred "missesArea" missesArea
  where
    testPred msg p = do
        testExamples msg p examples -- test all examples
        quickCheck $ putTestCase msg p -- test arbitrary values


-- | draw the nth example on the screen
showExample n =
    drawOffender Nothing (examples !! n)

-- | show all examples one after the other from n
showExamples n = forM_ [n .. (length examples - 1)] showExample

-- | shows an arbitrary offending value (if one can be found
showArbitraryOffender :: Testable p => (TestPolygons -> p) -> IO ()
showArbitraryOffender p =
    catcher $ quickCheck $ \ a -> whenFail (throwIO a) $ p a

-- | collection of problematic examples with increasing complexity
examples :: [TestPolygons]
examples = map (Wrap . map (Wrap . Polygon)) [
    [
        [Vector (-5.0) 2.0,Vector (-5.0) 5.0,Vector (-3.0) 5.0,Vector (-3.0) 2.0],
        [Vector (-5.0) 3.0,Vector (-5.0) 5.0,Vector (-4.0) 5.0,Vector (-4.0) 3.0]
    ],
    [
        [Vector 0.0 (-8.0),Vector 0.0 (-7.0),Vector 2.0 (-7.0),Vector 2.0 (-8.0)],
        [Vector 1.0 (-10.0),Vector 1.0 (-6.0),Vector 4.0 (-6.0),Vector 4.0 (-10.0)]
    ],
    [
        [Vector 0 0, Vector 0 5,Vector 2 5,Vector 2 0],
        [Vector 0 3,Vector 0 5,Vector 5 5,Vector 5 3]
    ],
    [
        [Vector (-10.0) (-9.0),Vector (-10.0) (-5.0),Vector (-5.0) (-5.0),Vector (-5.0) (-9.0)],
        [Vector (-5.0) (-10.0),Vector (-5.0) (-5.0),Vector 0.0 (-5.0),Vector 0.0 (-10.0)]
    ],
    [
        [Vector 2.0 5.0,Vector 2.0 9.0,Vector 5.0 9.0,Vector 5.0 5.0],
        [Vector 2.0 4.0,Vector 2.0 6.0,Vector 6.0 6.0,Vector 6.0 4.0]
    ],
    [
        [Vector 0.0 (-9.0),Vector 0.0 (-5.0),Vector 4.0 (-5.0),Vector 4.0 (-9.0)],
        [Vector 2.0 (-9.0),Vector 2.0 (-2.0),Vector 3.0 (-2.0),Vector 3.0 (-9.0)]
    ],
    [
        [Vector (-1.0) (-4.0),Vector (-1.0) (-3.0),Vector 4.0 (-3.0),Vector 4.0 (-4.0)],
        [Vector 4.0 (-7.0),Vector 4.0 (-3.0),Vector 9.0 (-3.0),Vector 9.0 (-7.0)]
    ],
    [
        [Vector (-1.0) 3.0,Vector (-1.0) 7.0,Vector 3.0 7.0,Vector 3.0 3.0],
        [Vector 0.0 3.0,Vector 0.0 7.0,Vector 4.0 7.0,Vector 4.0 3.0]
    ],
    [
        [Vector 1.0 (-7.0),Vector 1.0 (-6.0),Vector 6.0 (-6.0),Vector 6.0 (-7.0)],
        [Vector 1.0 (-10.0),Vector 1.0 (-6.0),Vector 2.0 (-6.0),Vector 2.0 (-10.0)],
        [Vector (-1.0) (-8.0),Vector (-1.0) (-6.0),Vector 3.0 (-6.0),Vector 3.0 (-8.0)]
    ],
    [
        [Vector 2.0 (-6.0),Vector 2.0 (-3.0),Vector 4.0 (-3.0),Vector 4.0 (-6.0)],
        [Vector 0.0 (-8.0),Vector 0.0 (-3.0),Vector 2.0 (-3.0),Vector 2.0 (-8.0)],
        [Vector (-2.0) (-3.0),Vector (-2.0) 1.0,Vector 2.0 1.0,Vector 2.0 (-3.0)]
    ],
    [
        [Vector (-3.0) (-10.0),Vector (-3.0) (-5.0),Vector 2.0 (-5.0),Vector 2.0 (-10.0)],
        [Vector 2.0 (-6.0),Vector 2.0 (-5.0),Vector 4.0 (-5.0),Vector 4.0 (-6.0)],
        [Vector (-4.0) (-8.0),Vector (-4.0) (-3.0),Vector 1.0 (-3.0),Vector 1.0 (-8.0)]
    ],
    [
        [Vector 5.0 0.0,Vector 5.0 4.0,Vector 7.0 4.0,Vector 7.0 0.0],
        [Vector 3.0 0.0,Vector 3.0 4.0,Vector 5.0 4.0,Vector 5.0 0.0],
        [Vector 4.0 2.0,Vector 4.0 3.0,Vector 7.0 3.0,Vector 7.0 2.0]
    ],
    [
        [Vector (-7.0) (-5.0),Vector (-7.0) (-2.0),Vector (-2.0) (-2.0),Vector (-2.0) (-5.0)],
        [Vector (-9.0) (-5.0),Vector (-9.0) 0.0,Vector (-5.0) 0.0,Vector (-5.0) (-5.0)],
        [Vector (-9.0) (-2.0),Vector (-9.0) 3.0,Vector (-8.0) 3.0,Vector (-8.0) (-2.0)]
    ],
    [
        [Vector (-2.0) 4.0,Vector (-2.0) 8.0,Vector (-1.0) 8.0,Vector (-1.0) 4.0],
        [Vector 1.0 4.0,Vector 1.0 5.0,Vector 2.0 5.0,Vector 2.0 4.0],
        [Vector (-2.0) 1.0,Vector (-2.0) 5.0,Vector 1.0 5.0,Vector 1.0 1.0]
    ],
    innerSquareExample,
    [
        [Vector (-4.0) (-3.0),Vector (-4.0) 1.0,Vector 0.0 1.0,Vector 0.0 (-3.0)],
        [Vector (-1.0) (-2.0),Vector (-1.0) 1.0,Vector 1.0 1.0,Vector 1.0 (-2.0)],
        [Vector (-6.0) 0.0,Vector (-6.0) 1.0,Vector (-4.0) 1.0,Vector (-4.0) 0.0],
        [Vector (-1.0) (-7.0),Vector (-1.0) (-2.0),Vector 0.0 (-2.0),Vector 0.0 (-7.0)]
    ],
    [
        [Vector 4.0 (-4.0),Vector 4.0 1.0,Vector 5.0 1.0,Vector 5.0 (-4.0)],
        [Vector 0.0 (-2.0),Vector 0.0 1.0,Vector 5.0 1.0,Vector 5.0 (-2.0)],
        [Vector (-1.0) (-4.0),Vector (-1.0) 1.0,Vector 4.0 1.0,Vector 4.0 (-4.0)]
    ],
    [
        [Vector (-3.0) 3.0,Vector (-3.0) 5.0,Vector 0.0 5.0,Vector 0.0 3.0],
        [Vector 0.0 4.0,Vector 0.0 5.0,Vector 2.0 5.0,Vector 2.0 4.0],
        [Vector (-2.0) 5.0,Vector (-2.0) 10.0,Vector 2.0 10.0,Vector 2.0 5.0]
    ],
    [
        [Vector 1.0 (-4.0),Vector 1.0 1.0,Vector 6.0 1.0,Vector 6.0 (-4.0)],
        [Vector 1.0 1.0,Vector 1.0 6.0,Vector 4.0 6.0,Vector 4.0 1.0],
        [Vector 1.0 (-5.0),Vector 1.0 (-2.0),Vector 2.0 (-2.0),Vector 2.0 (-5.0)]
    ]
  ]


innerSquareExample =
    big : nub (left ++ right ++ up ++ down)
  where
    standard = [Vector 0 0, Vector 0 1, Vector 1 1, Vector 1 0]
    big = map (flip scale 2 >>> (+~ Vector 1 1)) standard
    left = map (\ i -> map (+~ Vector 0 i) standard) [0 .. 3]
    right = map (\ i -> map (+~ Vector 3 i) standard) [0 .. 3]
    up = map (\ i -> map (+~ Vector i 0) standard) [0 .. 3]
    down = map (\ i -> map (+~ Vector i 3) standard) [0 .. 3]


