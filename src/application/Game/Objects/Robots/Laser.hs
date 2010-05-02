{-# language NamedFieldPuns, ViewPatterns #-}

module Game.Objects.Robots.Laser where


import Utils
import Constants

import Data.Map (Map, (!))
import Data.List
import qualified Data.Indexable as I
import Data.Abelian

import Control.Applicative ((<$>))
import Control.Monad hiding ((>=>))
import Control.Monad.Compose

import Physics.Chipmunk as CM

import Graphics.Qt as Qt

import Editor.Sprited

import Game.Objects
import Game.Objects.Types
import Game.Objects.Helper
import Game.Objects.Robots.Types
import Game.Objects.Robots.Handler(RobotHandler(RobotHandler))

import Game.Scene.Types
import Game.Scene.Grounds

import Game.Events

import Game.Collisions

import Game.Animation


laserRobotHandler :: RobotHandler
laserRobotHandler = RobotHandler
    initialisation
    initAnimation
    updating
    rendering


initialisation :: UninitializedScene -> Space -> UninitializedObject -> IO Object
initialisation scene space robot@(Robot s p state) = do
        let size = defaultPixmapSize s
            bodyAttributes = StaticBodyAttributes p
            shapeAttributes = ShapeAttributes{
                elasticity = 0.5,
                friction = robotFriction,
                collisionType = toCollisionType robot
              }
            (polys, baryCenterOffset) = mkStandardPolys size
            shapesAndAttributes = map (tuple shapeAttributes) polys

        chip <- CM.initStaticChipmunk space bodyAttributes shapesAndAttributes baryCenterOffset
        -- adding lasers
        chipMunkAddLasers scene space shapeAttributes p (Robot s chip state)

chipMunkAddLasers :: UninitializedScene -> Space -> ShapeAttributes -> Vector
    -> Object -> IO Object
chipMunkAddLasers scene space attrs pngPos (Robot s chip (LaserRobot lasers a b)) = do
    let directions = map laserDirection lasers
        lowerLeft = calcLocalLowerLeft directions
        innerRobotPosition = calcInnerRobotPosition directions pngPos
    (chip', lasers') <- myTupleMap (addLaser scene space innerRobotPosition lowerLeft) chip lasers
    return $ Robot s chip' (LaserRobot lasers' a b)

-- | calculates the offset of the lower left corner of the robot from the barycenter
-- (which is the middle of the png), such that
-- lowerLeftCorner == offset +~ laserBotPos
calcLocalLowerLeft :: [Direction] -> Qt.Position Double
calcLocalLowerLeft [DUp] = Position 0 (endBotLength / 2) +~ toLowerLeft
calcLocalLowerLeft [DUp, DRight] = Position (- endBotLength / 2)  (endBotLength / 2) +~ toLowerLeft
calcLocalLowerLeft x = es "calcMiddleLowerLeft" x

-- | distance from middle to lower left corner of the robot
toLowerLeft :: Qt.Position Double
toLowerLeft = Position (- fromUber 7.5) (fromUber 7.5)

-- | calculates the global position of the inner robot
calcInnerRobotPosition :: [Direction] -> Vector -> Qt.Position Double
calcInnerRobotPosition [DUp] p =
    vectorToPosition p +~ Position 0 (endBotLength + height robotSize)
calcInnerRobotPosition [DUp, DRight] p =
    vectorToPosition p +~ Position 0 (endBotLength + height robotSize)
calcInnerRobotPosition x _ = es "calcInnerRobotPosition" x

-- | what i need here. Like mapM in (StateM a m)
myTupleMap :: Monad m => (a -> b -> m (a, b)) -> a -> [b] -> m (a, [b])
myTupleMap cmd a [] = return (a, [])
myTupleMap cmd a (b : r) = do
    (a', b') <- cmd a b
    (a'', r') <- myTupleMap cmd a' r
    return (a'', b' : r')

addLaser :: UninitializedScene -> Space -> Qt.Position Double -> Qt.Position Double
    -> Chipmunk -> Laser -> IO (Chipmunk, Laser)
addLaser scene space laserBotPos lowerLeft chip
    (Laser direction Nothing switch 0) = do
        let (len, position, size) = calculateLaser scene laserBotPos lowerLeft direction
            shapeTypes = [mkRect position size]
        (chip', shapes) <- CM.addInitShape chip (map (tuple laserAttributes) shapeTypes)
        let laser = Laser direction (Just shapes) switch len
        when (not switch) $
            mapM_ (spaceRemove space . Static) shapes
        return (chip', laser)

laserAttributes :: ShapeAttributes
laserAttributes = ShapeAttributes 0.5 0.5 LaserCT

endBotLength, endBotBreite, laserBreite :: Double
endBotLength = fromUber 6
endBotBreite = fromUber 13
laserBreite = fromUber 5

-- | inner robot size
robotSize :: Size Double
robotSize = Size (fromUber 15) (fromUber 15)

-- | calculates the data for a laser
calculateLaser :: UninitializedScene -> Qt.Position Double -> Qt.Position Double -> Direction
    -> (Int, Qt.Position Double, Size Double)
calculateLaser scene laserBotPos lowerLeft DUp =
    (truncate len, laserPosition, Size laserBreite len)
  where
    laserPosition :: Qt.Position Double
    laserPosition = lowerLeft +~ Position (fromUber 5) (- height robotSize - endBotLength - len)

    len = withView positionY distance laserBotPos endBotPos - height robotSize - endBotLength - endBotLength

    endBotPos = findEndBot scene
        (\ p -> positionX p == positionX laserBotPos + fromUber 1
            && positionY p < positionY laserBotPos)
        (\ a b -> swapOrdering (compare a b), positionY) -- << (tuple laserBotPos)

calculateLaser scene laserBotPos lowerLeft DRight =
    (truncate len, laserPosition, Size len laserBreite)
  where
    len :: Double
    len = withView positionX distance laserBotPos endBotPos - width robotSize - endBotLength
    laserPosition = lowerLeft +~
        Position (width robotSize + endBotLength) (- height robotSize + fromUber 5)
    endBotPos = findEndBot scene
        (\ p -> positionY p == positionY laserBotPos - endBotBreite - fromUber 1
            && positionX p > positionX laserBotPos)
        (compare, positionX)
calculateLaser _ _ _ dir = es "calculateLaser" dir


findEndBot :: UninitializedScene
    -> (Qt.Position Double -> Bool)
    -> (a -> a -> Ordering, Qt.Position Double -> a)
    -> Qt.Position Double
findEndBot scene predicate (compare, getter) =
    endBotPos
  where
    os = objects scene
    mainLayerObjects = I.toList $ mainLayerIndexable os
    filtered = filter innerPredicate $
        filter isLaserEndRobot mainLayerObjects
    endBot = case filtered of
        (_ : _) -> minimumBy innerCompare filtered
        _ -> error "no laser end bot found"
    endBotPos = innerGetPos endBot

    -- versions with objects not positions as argument
    innerPredicate :: UninitializedObject -> Bool
    innerPredicate = predicate . innerGetPos
    innerCompare a b = withView innerGetter compare a b
    innerGetter = getter . innerGetPos

    innerGetPos :: UninitializedObject -> Qt.Position Double
    innerGetPos = vectorToPosition . chipmunk


initAnimation :: Object -> Object
initAnimation = modifyRobotState inner
  where
    inner (LaserRobot laser UninitializedAnimation UninitializedAnimation) =
        LaserRobot laser robotAnimation laserAnimation
    robotAnimation = mkAnimation (UndirectedFrameSetType Idle) robotFun 0
    laserAnimation = mkAnimation (UndirectedFrameSetType RedLaser) laserFun 0

    robotFun (frameSetAction -> Idle) = AnimationPhases $ zip
        (cycle [0 .. 3])
        (repeat robotIdleEyeTime)
    robotFun (frameSetAction -> Wait) = robotWaitAnimation

    laserFun (frameSetAction -> RedLaser) = AnimationPhases $ zip
        (cycle [0, 1])
        (repeat 0.02)

updating :: Scene -> Seconds -> Collisions -> (Bool, ControlData) -> Object -> IO Object
updating _ now collisions control object =
    modifyRobotStateM (
        updateLogick (space $ chipmunk object) control >=>
        pure (updateAnimations now control))
      object

updateLogick :: Space -> (Bool, ControlData)
    -> RobotState -> IO RobotState
updateLogick space (isControlled, cd) state =
    if isControlled && aPushed then
        switch state
      else
        return state
  where
    switch (LaserRobot lasers a b) = do
        new <- swapSwitches space lasers
        return $ LaserRobot new a b

    aPushed = Press AButton `elem` pushed cd

-- | swaps the switch state of one laser
swapSwitches :: Space
    -> [Laser] -> IO [Laser]
swapSwitches space lasers = do
    let swaps = case map laserDirection lasers of
            -- which shall be switched
            [DUp] -> [True]
            [DUp, DRight] -> [True, True]
            x -> es "swapSwitches" x
    mapM (uncurry inner) $ zip lasers swaps
  where
    inner :: Laser -> Bool -> IO Laser
    inner laser False = return laser
    inner (Laser direction (Just shapes) on len) True = do
        if on then
            mapM_ (spaceRemove space . Static) shapes
          else
            mapM_ (spaceAdd space . Static) shapes
        return $ Laser direction (Just shapes) (not on) len


updateAnimations :: Seconds -> (Bool, ControlData) -> RobotState -> RobotState
updateAnimations now (isControlled, _) (LaserRobot lasers robotAnimation laserAnimation) = 
    LaserRobot lasers robotAnimation' laserAnimation'
  where
    robotAnimation' = updateAnimation now robotAnimationType robotAnimation
    robotAnimationType = UndirectedFrameSetType $
         if isControlled then Wait else Idle

    laserAnimation' = updateAnimation now (UndirectedFrameSetType RedLaser) laserAnimation


rendering :: Ptr QPainter -> Scene -> Qt.Position Double -> Object -> IO ()
rendering ptr scene offset (Robot sprited chipmunk (LaserRobot lasers robotAnimation laserAnimation)) = do

    laserBotPos <- vectorToPosition <$> fst <$> getRenderPosition chipmunk
    mapM_ (renderLaser ptr offset laserBotPos laserAnimation (osdSpriteds scene)) lasers

    let pixmap = animationPixmap robotAnimation sprited
    renderChipmunk ptr offset pixmap chipmunk



-- | would need the position of the inner robot.
-- works for snowwhite like this
renderLaser :: Ptr QPainter -> Qt.Position Double -> Qt.Position Double -> Animation
    -> Map [Char] (Object_ Sprited t) -> Laser
    -> IO ()
renderLaser _ _ _ _ _ (Laser _ _ False _) = return ()
renderLaser ptr offset laserBotPos laserAnimation osdSpriteds laser = do
    resetMatrix ptr
    translate ptr offset
    translate ptr laserBotPos
    renderDirectedLaser laser
  where
    renderDirectedLaser (Laser DUp _ True len) = do
        translate ptr (Position (fromUber 1) (fromUber 1))
        forM_ [0 .. (len `div` 4) + 1] $ \ i -> do
            translate ptr (Position 0 (- 4))
            drawPixmap ptr zero (laserPix "laser-vertical")
    renderDirectedLaser (Laser DRight _ True len) = do
        translate ptr (Position (fromUber 19) (fromUber 7))
        forM_ [0 .. (len `div` 4) + 1] $ \ i -> do
            translate ptr (Position (fromUber 1) 0)
            drawPixmap ptr zero (laserPix "laser-horizontal")

    laserPix name =
        animationPixmap laserAnimation laserSprited
      where
        (OSDObject laserSprited) = osdSpriteds ! name


-- * laser end robot

laserEndRobotHandler :: RobotHandler
laserEndRobotHandler = RobotHandler
    endInitialisation
    id
    endUpdating
    endRendering

endInitialisation :: UninitializedScene -> Space -> UninitializedObject -> IO Object
endInitialisation _ space robot@(Robot s p state) = do
        let size = defaultPixmapSize s
            bodyAttributes = StaticBodyAttributes p
            shapeAttributes = ShapeAttributes{
                elasticity = 0.5,
                friction = robotFriction,
                collisionType = toCollisionType robot
              }
            (polys, baryCenterOffset) = mkStandardPolys size
            shapesAndAttributes = map (tuple shapeAttributes) polys

        chip <- CM.initStaticChipmunk space bodyAttributes shapesAndAttributes baryCenterOffset
        return $ Robot s chip state

endUpdating :: Scene -> Seconds -> Collisions -> (Bool, ControlData) -> Object -> IO Object
endUpdating _ _ _ _ = return

endRendering :: Ptr QPainter -> Scene -> Qt.Position Double -> Object -> IO ()
endRendering ptr _ offset (Robot sprited chipmunk state) = do
    let pixmap = defaultPixmap sprited
    renderChipmunk ptr offset pixmap chipmunk


