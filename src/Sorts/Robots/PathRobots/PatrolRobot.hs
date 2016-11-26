{-# language ViewPatterns, NamedFieldPuns, ScopedTypeVariables, MultiParamTypeClasses,
    DeriveDataTypeable #-}


module Sorts.Robots.PathRobots.PatrolRobot where


import Data.Typeable
import Data.Abelian
import Data.Data
import Data.Accessor

import System.FilePath

import Physics.Chipmunk as CM hiding (Position, radius)

import Graphics.Qt hiding (scale)

import Utils

import Base hiding (cursorStep)

import Sorts.Robots.Configuration
import Sorts.Robots.Eyes

import Sorts.Robots.PathRobots.Configuration
import Sorts.Robots.PathRobots.Path


sort :: RM Sort_
sort = do
    off <- load "standard-off"
    red <- load "standard-on_00"
    blue <- load "standard-on_01"
    let animation = mkAnimation [red, blue] (singleton patrolFrameTime)
    robotEyes <- loadRobotEyesPixmaps
    return $ Sort_ $ PSort off red blue animation robotEyes
  where
    load :: String -> RM Pixmap
    load name = do
        path <- getDataFileName (pngDir </> "robots" </> "patrol" </> name <.> "png")
        loadSymmetricPixmap (Position 21 21) path


data PSort = PSort {
    offPix :: Pixmap,
    redPix :: Pixmap,
    bluePix :: Pixmap,
    blinkAnimation :: Animation Pixmap,
    robotEyes :: RobotEyesPixmaps
  }
    deriving (Show, Typeable, Data)

data Patrol
    = Patrol {
        robotSize :: Size Double,
        chipmunk :: Chipmunk,
        path :: Path,
        deadly_ :: Bool
      }
  deriving (Show, Typeable)

deadly :: Accessor Patrol Bool
deadly = accessor deadly_ (\ a r -> r{deadly_ = a})


instance Sort PSort Patrol where
    sortId _ = SortId "robots/platform/patrol"
                             -- one bigger than actual (to prevent getting stuck on edges)
    size _ = fmap fromUber $ Size 28 28

    objectEditMode s = Just $ oemMethods $ size s

    renderIconified sort ptr = do
        translate ptr physicsPadding
        renderPixmapSimple ptr (offPix sort)

    renderEditorObject ptr offset (EditorObject sort position (Just (OEMState oemState))) =
        case cast oemState of
            (Just oemPath :: Maybe OEMPath) -> do
                resetMatrix ptr
                translate ptr offset
                renderOEMPath (size sort) ptr offset $ getPathList $ pathPositions oemPath
                translate ptr (epToPosition (size sort) position)
                translate ptr physicsPadding
                let pix = if oemActive oemPath then redPix sort else offPix sort
                renderPixmapSimple ptr pix

    initialize _app _ (Just space) sort ep (Just (OEMState oemState_)) _ = io $ do
        let Just oemState = cast oemState_
            baryCenterOffset = size2vector $ fmap (/ 2) $ size sort

            shape = mkPoly sort
            deadly = oemActive oemState
            shapes = [mkShapeDescription (shapeAttributes deadly) shape]

            pos = position2vector (epToPosition (size sort) ep)
                    +~ baryCenterOffset
        chip <- initChipmunk space (bodyAttributes sort pos) shapes baryCenterOffset

        let path = toPath (size sort) oemState
        return $ Patrol (size sort) chip path deadly
    initialize _app _ Nothing sort ep _ _ = do
        let baryCenterOffset = size2vector $ fmap (/ 2) $ size sort
            position = epToPosition (size sort) ep
            vector = position2vector position +~ baryCenterOffset
            path = mkPath False [vector]
            chip = ImmutableChipmunk position 0 baryCenterOffset []
        return $ Patrol (size sort) chip path True

    chipmunks p = [chipmunk p]

    getControlledChipmunk = const chipmunk

    immutableCopy p@Patrol{chipmunk} =
        CM.immutableCopy chipmunk >>= \ x -> return p{chipmunk = x}

    isUpdating = const True

    updateNoSceneChange _sort _ config _ _mode _now _contacts cd =
        control (config ^. controls) cd >=>
        return . updateLogic >=>
        passThrough applyPatrolForce

    renderObject _ _ patrol sort _ptr _offset now = do
        (position, rad) <- getRenderPositionAndAngle $ chipmunk patrol
        let renderPosition = position +~ physicsPadding
            robot = RenderPixmap (pickRobotPixmap now sort patrol) renderPosition Nothing
            eyes = renderRobotEyes (robotEyes sort) renderPosition rad eyesOffset
                (robotEyesState patrol) now
        return (robot : eyes : [])


-- | To prevent ppatrol robots from getting stuck everywhere, we
-- use a padding of 0.5 Überpixels.
physicsPadding :: Position Double
physicsPadding = fmap fromUber $ Position 0.5 0.5

-- * attributes

mkPoly :: PSort -> ShapeType
mkPoly sort = mkRect
    (Position (- width / 2) (- height / 2) +~ fmap realToFrac physicsPadding)
    (size_ -~ fmap fromUber (Size 1 1))
  where
    size_ :: Size CpFloat
    size_@(Size width height) = fmap realToFrac $ size sort

bodyAttributes :: PSort -> Vector -> BodyAttributes
bodyAttributes _sort pos =
    BodyAttributes pos patrolMass infinity

-- | tile friction to allow better walking
shapeAttributes deadly =
    ShapeAttributes
        (elasticity robotShapeAttributes)
        patrolFriction
        (if deadly then DeadlySolidCT else RobotCT)


-- * initialising

-- | reads an OEMPath and returns the path for the game
toPath :: Size Double -> OEMPath -> Path
toPath size (OEMPath _ _ _cursor path _) =
    mkPath True $ map (epToCenterVector size) (getPathList path)

-- * controlling

control :: Controls -> (Bool, ControlData) -> Patrol -> IO Patrol
control config (True, cd) | isRobotActionPressed config cd =
    return . (deadly ^: not) >=>
    passThrough updateCollisionType
control _ _ = return

-- | Updates the collision type of the patrol robot.
-- DeadlySolidCT or RobotCT
updateCollisionType :: Patrol -> IO ()
updateCollisionType p =
    mapM_ (setMyCollisionType ct) $ shapes $ chipmunk p
  where
    ct = if p ^. deadly then DeadlySolidCT else RobotCT

-- * physics behaviour

updateLogic :: Patrol -> Patrol
updateLogic patrol@Patrol{path} =
    patrol{path = updatePath path}

applyPatrolForce :: Patrol -> IO ()
applyPatrolForce p = applyPathRobotForce (chipmunk p) (path p)


-- * rendering

pickRobotPixmap :: Seconds -> PSort -> Patrol -> Pixmap
pickRobotPixmap now sort patrol =
    if patrol ^. deadly then activePix else offPix sort
  where
    activePix = pickAnimationFrame (blinkAnimation sort) now

-- | Returns the state of the robots eyes dependent on the current Path
robotEyesState :: Patrol -> RobotEyesState
robotEyesState p =
    if p ^. deadly then Active else Idle

eyesOffset = fmap fromUber $ Position 8 9
