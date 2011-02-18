{-# language DeriveDataTypeable #-}

-- | module for rendering the eyes of the robots

module Sorts.Robots.Eyes where


import Data.Generics

import Text.Printf

import System.FilePath

import Graphics.Qt

import Utils

import Base


data RobotEyesPixmaps = RobotEyesPixmaps {
    actives :: [Pixmap],
    idles :: [Pixmap]
  } deriving (Show, Typeable, Data)

-- | will be called by every robot sort, but sharing takes place on Qt's side.
loadRobotEyesPixmaps :: RM RobotEyesPixmaps
loadRobotEyesPixmaps = do
    actives <- loadImages "active" 2
    idles <- loadImages "idle" 4
    return $ RobotEyesPixmaps actives idles
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

data RobotEyesState
    = Idle
    | Active
  deriving Show

renderRobotEyes :: RobotEyesPixmaps -> Ptr QPainter -> Offset Double -> Position Double
    -> RobotEyesState -> IO ()
renderRobotEyes pixmaps ptr offset pos state = do
    resetMatrix ptr
    translate ptr offset
    translate ptr pos
    renderPixmapSimple ptr (head $ actives pixmaps)
