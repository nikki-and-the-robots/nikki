{-# language DeriveDataTypeable #-}

-- | module for rendering the eyes of the robots

module Sorts.Robots.Eyes (
    RobotEyesPixmaps,
    loadRobotEyesPixmaps,
    RobotEyesState(..),
    renderRobotEyes,
  ) where


import Data.Generics
import Data.Map
import Data.Abelian

import Text.Printf

import System.FilePath

import Physics.Chipmunk (Angle)

import Graphics.Qt

import Utils

import Base


-- * configuration

animationFrameTimes :: Map RobotEyesState [Seconds]
animationFrameTimes = fromList (
    (Idle, [0.4]) :
    (Active, [3, 0.15, 0.1, 0.15]) :
    (Open, [42]) :
    (Closed, [42]) :
    [])


-- * loading

type RobotEyesPixmaps = Map RobotEyesState [Pixmap]

-- | will be called by every robot sort, but sharing takes place on Qt's side.
loadRobotEyesPixmaps :: RM RobotEyesPixmaps
loadRobotEyesPixmaps = do
    actives <- loadImages "active" 2
    idles <- loadImages "idle" 4
    let open = actives !! 0
        closed = actives !! 1
    return $ fromList (
        (Active, actives) :
        (Idle, idles) :
        (Open, [open]) :
        (Closed, [closed]) :
        [])
  where
    -- load n images with the given name
    -- e.g. "png/robots/eyes/idle_00.png" to "...idle_01.png"
    loadImages :: String -> Int -> RM [Pixmap]
    loadImages name n =
        forM [0 .. n - 1] $ \ i -> do
            path <- getDataFileName
                (pngDir </> "robots" </> "eyes" </> name `underscore` printf "%02i" i <.> "png")
            loadPixmap (Position 1 1) path
    underscore a b = a ++ "_" ++ b


-- * rendering

data RobotEyesState
    = Idle
    | Active
    | Open
    | Closed
  deriving (Eq, Ord, Show, Typeable, Data)

-- | Renders the eyes of a robot.
renderRobotEyes :: RobotEyesPixmaps
    -> Position Double -> Angle -> Position Double
    -> RobotEyesState -> Seconds -> RenderPixmap
renderRobotEyes pixmaps pos angle eyesOffset state stateTime =
    RenderPixmap
        (pixmapOffset ^: (+~ eyesOffset) $ pickPixmap pixmaps state stateTime)
        pos
        (Just angle)

pickPixmap :: RobotEyesPixmaps -> RobotEyesState -> Seconds -> Pixmap
pickPixmap pixmaps state stateTime =
    pickAnimationFrame (pixmaps ! state) (animationFrameTimes ! state) stateTime
