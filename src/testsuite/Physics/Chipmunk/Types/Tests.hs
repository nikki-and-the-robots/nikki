

module Physics.Chipmunk.Types.Tests where


import Data.Abelian

import Test.QuickCheck

import Physics.Chipmunk
import Physics.Chipmunk.Tests ()

import Utils

import Utils.Tests


tests :: IO ()
tests = do
    testMassForShape
    testComponent

-- * massForShape

testMassForShape = do
    quickCheck $ shapeTest mkCircle
    quickCheck $ shapeTest mkTriangle
    quickCheck $ shapeTest mkSquare

shapeTest :: Gen (ShapeType, Mass) -> Property
shapeTest shapeGen =
    forAll shapeGen $ \ (shape, area) ->
    forAll mkMass $ \ mpp ->
    let result = massForShape mpp shape
        expected = area * mpp
    in printTestCase (show (expected, result))
        (result =~ expected)

a =~ b = abs (a - b) < eps
eps = 0.000001

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


-- * component

testComponent = do
    quickCheckOnce $ toUpAngleZero
    quickCheck $ componentSameAngle
    quickCheck $ componentUpAngleSameAngle

toUpAngleZero =
    printTestCase "toUpAngleZero" $
    (toUpAngle zero == 0 &&
     toUpAngle (Vector 0 (- 0)) == 0)

componentUpAngleSameAngle angle vector =
    printTestCase "componentUpAngleSameAngle" $
    let expected = foldAngle angle
        result = foldAngle (toUpAngle (componentUpAngle angle vector))
    in printTestCase (show expected ++ " /~%= " ++ show result) $
        (expected ~~= result ||
        (expected + (deg2rad 180)) ~~= result)
  where
    (~~=) = withView foldAngle (~=)

componentSameAngle angle vector =
    printTestCase "componentSameAngle" $
    let expected = foldAngle angle
        result = foldAngle (toAngle (component angle vector))
    in printTestCase (show expected ++ " /~%= " ++ show result) $
        (expected ~~= result ||
        (expected + (deg2rad 180)) ~~= result)
  where
    (~~=) = withView foldAngle (~=)
