{-# language FlexibleInstances, NamedFieldPuns #-}

module Game.Animation (
    Seconds,
    Animation(UninitializedAnimation, SingleFrameAnimation, animationType, frameNumber),
    AnimationPhases(..),
    isUninitializedAnimation,
    getFrameSetAction,
    mkAnimation,
    updateAnimation,
    animationPixmap,
  ) where

import Utils

import Graphics.Qt

import Editor.Sprited


type Seconds = Double

data Animation
    = UninitializedAnimation
    | SingleFrameAnimation -- dummy Animation
    | Animation {
        animationType :: FrameSetType,
        frameNumber :: Int,
        frameDuration :: Seconds,
        followingFrame :: Seconds -> Animation,
        newAnimation :: FrameSetType -> AnimationPhases,
        frameStartTime :: Seconds
      }
    | StillFrameAnimation {
        animationType :: FrameSetType,
        frameNumber :: Int,
        newAnimation :: FrameSetType -> AnimationPhases
      }
  deriving Show

instance Show (Seconds -> Animation) where
    show _ = "<followingFrame>"

instance Show (FrameSetType -> AnimationPhases) where
    show = const "<newAnimation>"


data AnimationPhases
    = StillFrame Int
    | AnimationPhases [(Int, Seconds)]


isUninitializedAnimation :: Animation -> Bool
isUninitializedAnimation UninitializedAnimation = True
isUninitializedAnimation _ = False

getFrameSetAction :: Animation -> FrameSetAction
getFrameSetAction a = frameSetAction $ animationType a


mkAnimation :: FrameSetType -> (FrameSetType -> AnimationPhases) -> Seconds -> Animation
mkAnimation typ fun = inner $ fun typ
  where
    inner (AnimationPhases ((frameNumber, duration) : rest)) =
        Animation typ frameNumber duration (inner (AnimationPhases rest)) fun
    inner (StillFrame frameNumber) =
        const $ StillFrameAnimation typ frameNumber fun


updateAnimation :: Seconds -> FrameSetType -> Animation -> Animation
updateAnimation secs newType animation | newType /= animationType animation =
    mkAnimation newType (newAnimation animation) secs
updateAnimation now newType a@(Animation typ number duration following _ startTime) =
    if now - startTime < duration then a else following now
updateAnimation _ _ a@StillFrameAnimation{} = a
updateAnimation s t a = es "updateAnimation" a

animationPixmap :: Animation -> Sprited -> Ptr QPixmap
animationPixmap SingleFrameAnimation sprited = defaultPixmap sprited
animationPixmap (StillFrameAnimation typ frameNumber _) sprited =
    getPixmap sprited typ frameNumber
animationPixmap Animation{animationType, frameNumber} sprited =
    getPixmap sprited animationType frameNumber
animationPixmap a _ = es "animationPixmap" a


