{-# language ViewPatterns, NamedFieldPuns, ScopedTypeVariables, MultiParamTypeClasses,
    DeriveDataTypeable #-}


module Sorts.Robots.PathRobots.Platform where


import Data.Typeable
import Data.Abelian
import Data.Data

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
sort = io $ do
    path <- getDataFileName (pngDir </> "robots" </> "platform" </> "horizontal-standard_standard_00" <.> "png")
    pix <- loadSymmetricPixmap (Position 1 1) path
    robotEyes <- loadRobotEyesPixmaps
    return $ Sort_ $ PSort pix robotEyes

data PSort = PSort {
    pix :: Pixmap,
    robotEyes :: RobotEyesPixmaps
  }
    deriving (Show, Typeable, Data)

data Platform
    = Platform {
        platformSize :: Size Double,
        chipmunk :: Chipmunk,
        path :: Path
      }
  deriving (Show, Typeable)


instance Sort PSort Platform where
    sortId _ = SortId "robots/platform/standard"
                             -- one bigger than actual (to prevent getting stuck on edges)
    size _ = fmap fromUber $ Size 48 22

    objectEditMode s = Just $ oemMethods $ size s

    renderIconified sort ptr = do
        translate ptr physicsPadding
        renderPixmapSimple ptr (pix sort)

    renderEditorObject ptr offset (EditorObject sort position (Just (OEMState oemState))) =
        case cast oemState of
            (Just oemPath :: Maybe OEMPath) -> do
                resetMatrix ptr
                translate ptr offset
                renderOEMPath (size sort) ptr offset $ getPathList $ pathPositions oemPath
                translate ptr (epToPosition (size sort) position)
                renderIconified sort ptr
                let renderPosition = (epToPosition (size sort) $
                        startPosition $ pathPositions oemPath) +~ physicsPadding
                    eyesState = if oemActive oemPath then Open else Closed
                doRenderPixmaps ptr $ singleton $ renderRobotEyes (robotEyes sort)
                    (renderPosition +~ offset) 0 eyesOffset eyesState 0

    initialize _app _ (Just space) sort ep (Just (OEMState oemState_)) _ = io $ do
        let Just oemState = cast oemState_
            baryCenterOffset = size2vector $ fmap (/ 2) $ size sort

            shape = mkPoly sort
            shapes = [mkShapeDescription shapeAttributes shape]

            pos = position2vector (epToPosition (size sort) ep)
                    +~ baryCenterOffset
        chip <- initChipmunk space (bodyAttributes sort pos) shapes baryCenterOffset

        let path = toPath (size sort) oemState
        return $ Platform (size sort) chip path
    initialize _app _ Nothing sort ep _ _ = do
        let baryCenterOffset = size2vector $ fmap (/ 2) $ size sort
            position = epToPosition (size sort) ep
            vector = position2vector position +~ baryCenterOffset
            path = mkPath False [vector]
            chip = ImmutableChipmunk position 0 baryCenterOffset []
        return $ Platform (size sort) chip path

    chipmunks p = [chipmunk p]

    getControlledChipmunk = const chipmunk

    immutableCopy p@Platform{chipmunk} =
        CM.immutableCopy chipmunk >>= \ x -> return p{chipmunk = x}

    isUpdating = const True

    updateNoSceneChange _sort _ config _ _mode _now _contacts cd =
        control (config ^. controls) cd >=>
        return . updateLogic >=>
        passThrough applyPlatformForce

    renderObject _ _ platform sort _ptr _offset now = do
        (position, rad) <- getRenderPositionAndAngle $ chipmunk platform
        let renderPosition = position +~ physicsPadding
            robot = RenderPixmap (pix sort) renderPosition Nothing
            eyes = renderRobotEyes (robotEyes sort) renderPosition rad eyesOffset
                (robotEyesState $ path platform) now
        return (robot : eyes : [])

-- | Returns the state of the robots eyes dependent on the current Path
robotEyesState :: Path -> RobotEyesState
robotEyesState p = case p of
    Path{} -> Active
    (SingleNode _ Nothing) -> Idle
    (SingleNode _ (Just _)) -> Idle

eyesOffset = fmap fromUber $ Position 18 9

-- | To prevent platforms from getting stuck everywhere, we
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
    BodyAttributes pos platformMass infinity

-- | tile friction to allow better walking
shapeAttributes = robotShapeAttributes{friction = platformFriction}

-- | reads an OEMPath and returns the path for the game
toPath :: Size Double -> OEMPath -> Path
toPath size (OEMPath _ _ _cursor path active) =
    mkPath active $ map (epToCenterVector size) (getPathList path)


-- * controlling

control :: Controls -> (Bool, ControlData) -> Platform -> IO Platform
control config (True, cd) platform | isRobotActionPressed config cd = do
    newPath <- swapOnOffState (chipmunk platform) $ path platform
    return platform{path = newPath}
control _ _ platform = return platform

swapOnOffState :: Chipmunk -> Path -> IO Path
swapOnOffState _ p@(SingleNode _ Nothing) = return p
swapOnOffState _ (SingleNode _ (Just p)) = return p
swapOnOffState c path@Path{} = do
    position <- getPosition c
    return $ SingleNode position (Just path)

-- * physics behaviour

updateLogic :: Platform -> Platform
updateLogic platform@Platform{path} =
    platform{path = updatePath path}

applyPlatformForce :: Platform -> IO ()
applyPlatformForce p = applyPathRobotForce (chipmunk p) (path p)
