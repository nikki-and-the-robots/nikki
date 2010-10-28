{-# language ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances, ViewPatterns,
    DeriveDataTypeable #-}

module Physics.Chipmunk.StickyEdges.Tests where


import Prelude hiding (catch)

import Data.Initial
import qualified Data.Indexable as I
import Data.Abelian
import Data.Typeable
import Data.List

import Control.Monad
import Control.Applicative ((<$>))
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


-- representation of edges

data Edge = Edge {
    from :: Vector,
    to :: Vector
  }
    deriving Show

rotateEdge (Edge (Vector a b) (Vector x y)) = Edge (Vector (- b) a) (Vector (- y) x)

swapEdge :: Edge -> Edge
swapEdge (Edge a b) = Edge b a

delta :: Edge -> Vector
delta (Edge a b) = b -~ a

edgeAngle :: Edge -> Angle
edgeAngle e = foldAngle $ toAngle $ delta e

hasStickyEdges :: [ShapeType] -> Bool
hasStickyEdges l = any (uncurry stickyEdges) $ completeEdges $ concat $ map toEdges l

toEdges :: ShapeType -> [Edge]
toEdges (Polygon l) = map (uncurry Edge) $ adjacentCyclic l

stickyEdges :: Edge -> Edge -> Bool
stickyEdges a b =
    (edgeAngle a == edgeAngle b &&
    (inner a b || inner (rotateEdge a) (rotateEdge b)))
  where
    inner a b = innerHorizontal a b || innerHorizontal b a
    innerHorizontal a b =
        horizontal a &&
        horizontal b &&
        vectorY (from a) == vectorY (from b) &&
        (vectorX (from a) `inside` (minB b, maxB b) ||
         vectorX (to a) `inside` (minB b, maxB b))
    horizontal a = vectorY (delta a) == 0
    minB b = min (vectorX (from b)) (vectorX (to b))
    maxB b = max (vectorX (from b)) (vectorX (to b))

testStickyEdges :: Bool
testStickyEdges =
    stickyEdges (Edge (Vector 9 4) (Vector 9 0)) (Edge (Vector 9 3) (Vector 9 2))


-- wrapper type in order to be able to put polygons in class Arbitrary whith special constraints:
-- ShapeTypes are  Polygons with four points forming a rectangle, starting with the upper left point,
-- continuing counterclockwise.
newtype Wrap a = Wrap {unwrap :: a}
  deriving (Show, Typeable)

type TestPolygons = Wrap [Wrap ShapeType]

instance Exception TestPolygons

fromTestPolygons :: TestPolygons -> [ShapeType]
fromTestPolygons = unwrap >>> map unwrap

instance Arbitrary TestPolygons where
    arbitrary = Wrap <$> listOf1 arbitrary
    shrink (Wrap l) = map Wrap $ shrink l

instance Arbitrary (Wrap ShapeType) where
    arbitrary = do
        x <- elements [-rectLimit .. (rectLimit - rectSize)]
        y <- elements [-rectLimit .. (rectLimit - rectSize)]
        width <- elements [1 .. rectSize]
        height <- elements [1 .. rectSize]
        return $ Wrap $ Polygon $ [
            Vector x y,
            Vector x (y + height),
            Vector (x + width) (y + height),
            Vector (x + width) y
          ]

rectSize :: Double = 5
rectLimit :: Double = 10
testEpsilon :: Double = 0.5



-- * drawing of offending values

catcher :: IO () -> IO ()
catcher = flip catch $ \ x -> drawOffender x

-- | scale a Vector that it will be visible on the screen
scaleVector :: Size Double -> Vector -> Vector
scaleVector (Size width height) = (+~ Vector rectLimit rectLimit) >>> (flip scale factor) >>> (+~ Vector padding padding)
  where
    factor = min xFactor yFactor
    xFactor = (width - (2 * padding)) / (4 * rectLimit)
    yFactor = (height - (2 * padding)) / (2 * rectLimit)
    padding = 30


drawOffender :: TestPolygons -> IO ()
drawOffender (fromTestPolygons -> offender) = do
    print offender
    withQApplication $ \ qApp -> do
        window <- newAppWidget 0
        keyPoller <- newKeyPoller window
        randoms <- generateRandoms
        setDrawingCallbackAppWidget window $ Just (render (cycle randoms))
        setWindowSize window $ Windowed (Size 1000 500)
        showAppWidget window
        execQApplication qApp
        return ()
  where
    render randoms ptr = do
        resetMatrix ptr
        windowSize <- sizeQPainter ptr
        let ws = fmap fromIntegral windowSize
        eraseRect ptr zero windowSize (QtColor 32 64 128 255)
        setPenColor ptr 255 255 55 255 1
        drawText ptr (Position 30 30) False (show (length offender))
        mapM_ id $ zipWith (renderShape ptr) randoms $ map (mapVectors (scaleVector ws)) offender
        mapM_ (\ (Edge a b) -> renderStickyVectorLine ptr (scaleVector ws a) (scaleVector ws b))
            $ concatMap (\ (a, b) -> [a, b]) $
            filter (uncurry stickyEdges) $ completeEdges $ concatMap toEdges offender
        resetMatrix ptr
        let traversed = removeStickyEdges testEpsilon offender
        drawText ptr (Position 60 30) False (show (length traversed))
        mapM_ (mapM_ (uncurry (renderPolygonLine ptr)) . adjacentCyclic . vertices) $
            map (mapVectors (scaleVector ws . (+~ Vector (rectLimit * 2) 0))) $
            traversed

generateRandoms =
    mapM (const inner) [1 .. 1000]
  where
    inner = do
        let rand = randomRIO (0, 255)
        r <- rand
        g <- rand
        b <- rand
        if r + g + b > 127 then
            return (r, g, b)
          else
            inner

renderShape ptr (r, g, b) (Polygon vertices) = do
    resetMatrix ptr
    translate ptr (vector2QtPosition $ head vertices)
    let Vector w h = vertices !! 2 -~ head vertices
    eraseRect ptr zero (fmap round $ Size w h) (QtColor r g b 127)
    resetMatrix ptr
    mapM_ (uncurry (renderPolygonLine ptr)) (adjacentCyclic vertices)

renderPolygonLine ptr a b = do
    renderCorner ptr a
    renderVectorLine ptr a b

renderCorner ptr (Vector x y) = do
    setPenColor ptr 255 255 55 255 1
    drawCircle ptr (Position x y) 3

renderVectorLine :: Ptr QPainter -> Vector -> Vector -> IO ()
renderVectorLine ptr (Vector x1 y1) (Vector x2 y2) = do
    setPenColor ptr 55 255 55 255 1
    drawLine ptr (Position x1 y1) (Position x2 y2)

renderStickyVectorLine :: Ptr QPainter -> Vector -> Vector -> IO ()
renderStickyVectorLine ptr (Vector x1 y1) (Vector x2 y2) = do
    setPenColor ptr 255 55 55 255 1
    drawLine ptr (Position x1 y1) (Position x2 y2)


-- * execution of tests

-- | composition of all tests in this module
tests :: Property
tests =
    testStickyEdges .&.
    testExamples .&.
    testArbitraries

-- | draw the nth example on the screen
showExample n = do
    putStrLn ("example nr.: " ++ show n ++ " --> " ++ show (predStickyEdges (examples !! n)))
    catcher $ quickCheck $ mkProperty (const False) (examples !! n)

-- | show all examples one after the other from n
showExamples n = forM_ [n .. (length examples - 1)] showExample

-- | tests the removal of sticky edges for all examples
testExamples :: IO ()
testExamples =
    inner 0 examples
  where
    inner i (example : r) = do
        putStrLn ("example nr.: " ++ show i)
        quickCheck $ mkProperty predStickyEdges example
        inner (i + 1) r
    inner i [] = return ()

testArbitraries :: Property
testArbitraries = property $ mkProperty predStickyEdges

-- | creates a quickcheck Property for a given predicate and set of polygons
mkProperty :: (TestPolygons -> Bool) -> TestPolygons -> Property
mkProperty p a = whenFail (throwIO a) $ p a

-- | this is the actual predicate, that 'removeStickyEdges' should ensure.
predStickyEdges :: TestPolygons -> Bool
predStickyEdges = not . hasStickyEdges . removeStickyEdges testEpsilon . fromTestPolygons

-- | collection of problematic examples with increasing complexity
examples :: [TestPolygons]
examples = map (Wrap . map (Wrap . Polygon)) ([
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
    ]
  ] +: innerSquareExample)


innerSquareExample =
    big : nub (left ++ right ++ up ++ down)
  where
    standard = [Vector 0 0, Vector 0 1, Vector 1 1, Vector 1 0]
    big = map (flip scale 2 >>> (+~ Vector 1 1)) standard
    left = map (\ i -> map (+~ Vector 0 i) standard) [0 .. 3]
    right = map (\ i -> map (+~ Vector 3 i) standard) [0 .. 3]
    up = map (\ i -> map (+~ Vector i 0) standard) [0 .. 3]
    down = map (\ i -> map (+~ Vector i 3) standard) [0 .. 3]


