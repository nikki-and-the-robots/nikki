

module Physics.Chipmunk.Types.Tests where


import Data.Abelian

import Test.QuickCheck

import Physics.Chipmunk
import Physics.Chipmunk.Tests ()

import Utils


tests :: IO ()
tests = do
    testMassForShape

-- * massForShape

testMassForShape = do
    quickCheck $ shapeTest mkCircle
    quickCheck $ shapeTest mkTriangle
    quickCheck $ shapeTest mkSquare

shapeTest :: Gen (ShapeType, Mass) -> Property
shapeTest shapeGen =
    forAll shapeGen $ \ (shape, expected) ->
    forAll mkMass $ \ mpp ->
    (massForShape mpp shape == expected * mpp)

mkMass :: Gen Mass
mkMass = choose (1, 100)

-- | translates and rotates a given polygon by random values.
randomizePolygon :: [Vector] -> Gen [Vector]
randomizePolygon vectors = do
    -- random translation
    t <- arbitrary
    -- random rotation
    rot <- choose (- pi, pi)
    -- translate and rotate the 
    return $ map (\ v -> rotate v (fromAngle rot) +~ t) vectors



-- | generate shapes and their areas
mkCircle, mkTriangle, mkSquare :: Gen (ShapeType, Double)
mkCircle = do
    radius <- choose (1, 100)
    let area = 2 * pi * radius
    return $ (Circle radius, area)

mkTriangle = do
    a <- choose (1, 100)
    h <- choose (1, 100)
    xA <- choose (-100, 100)
    -- a normalized triangle
    let normalized = [Vector 0 0, Vector a 0, Vector xA h]
        area = a * h / 2
    randomized <- Polygon <$> randomizePolygon normalized
    return (randomized, area)

mkSquare = do
    a <- choose (1, 100)
    b <- choose (1, 100)
    let normalized = [Vector 0 0, Vector a 0, Vector a b, Vector 0 b]
        area = a * b
    randomized <- Polygon <$> randomizePolygon normalized
    return (randomized, area)
