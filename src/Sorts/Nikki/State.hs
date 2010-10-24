{-# language ViewPatterns #-}

module Sorts.Nikki.State where


import Prelude hiding (lookup)

import Data.List (sortBy)
import Data.Set (member)
import Data.Abelian
import Data.Maybe

import Control.Arrow
import Control.Applicative ((<|>))

import Graphics.Qt as Qt hiding (rotate, scale)

import qualified Physics.Chipmunk as CM
import Physics.Chipmunk hiding (position, Position)

import Utils

import Base.Constants
import Base.Events
import Base.Directions
import Base.Types

import Sorts.Nikki.Types
import Sorts.Nikki.Configuration


updateState :: Seconds -> Contacts -> (Bool, ControlData) -> Nikki -> IO Nikki
updateState _ _ (False, _) nikki =
    return $ nikki{state = State UsingTerminal (direction $ state nikki)}
updateState now contacts (True, controlData) nikki = do
    velocity_ <- get $ velocity $ body $ chipmunk nikki
    nikkiPos <- getPosition $ chipmunk nikki
    return $ nikki{state = state' nikkiPos velocity_}
  where
    state' nikkiPos velocity_ =
        case (aPushed, mContactAngle) of
            -- nikki jumps
            (True, Just contactAngle) -> State (JumpImpulse now contactAngle buttonDirection velocity_)
                                            (jumpImpulseDirection contactAngle)
            -- nikki touches something
            (False, Just contactAngle) ->
                case grips nikkiPos contacts of
                    -- nikki grabs something
                    Just HLeft | rightPushed -> State EndGripImpulse HLeft
                    Just HRight | leftPushed -> State EndGripImpulse HRight
                    Just gripDirection -> State Grip gripDirection
                    -- nikki grabs nothing
                    Nothing ->
                        if nikkiFeetTouchGround contacts then
                            if nothingHeld then
                                State Wait newDirection
                              else
                                State Walk newDirection
                          else
                            State (WallSlide (jumpInformation velocity_)
                                (clouds nikkiPos newDirection))
                                newDirection
            (_, Nothing) ->
                State (Airborne (jumpInformation velocity_)) newDirection

    newDirection :: HorizontalDirection
    newDirection = case state nikki of
        State Grip direction -> swapHorizontalDirection direction
        _ -> fromMaybe oldDirection buttonDirection
    grips :: CM.Position -> Contacts -> Maybe HorizontalDirection
    grips nikkiPos contacts = case filter (isGripCollision nikkiPos) (nikkiContacts contacts) of
        [] -> Nothing
        (Collision _ (p : _) : _) -> Just $
            if vectorX (p -~ nikkiPos) <= 0 then HLeft else HRight
    isGripCollision nikkiPos (Collision normal points) =
        any (isGripPoint nikkiPos) points && isGripNormal normal
    isGripPoint nikkiPos p = vectorY (p -~ nikkiPos) =~= 19
    isGripNormal ((toUpAngle >>> foldAngle) -> angle) =
        (angle > (- angleLimit)) && (angle < angleLimit)
    angleLimit = deg2rad 45
    a =~= b = abs (a - b) < eps
    eps = 1


    aPushed = Press AButton `elem` pressed controlData
    aHeld = AButton `member` held controlData
    rightPushed = Press RightButton `elem` pressed controlData
    leftPushed = Press LeftButton `elem` pressed controlData
    rightHeld = RightButton `member` held controlData
    leftHeld = LeftButton `member` held controlData
    nothingHeld = not (rightHeld `xor` leftHeld)

    oldDirection :: HorizontalDirection
    oldDirection = direction $ state nikki
    buttonDirection :: Maybe HorizontalDirection
    buttonDirection =
        if nothingHeld then Nothing else
        if leftHeld then Just HLeft else
        Just HRight
    verticalDirection velocity_ = if vectorY velocity_ <= 0 then VUp else VDown

    mContactAngle :: Maybe Angle
    mContactAngle = jumpAngle $ getContactNormals contacts

    jumpInformation velocity =
        JumpInformation jumpStartTime_ buttonDirection velocity (verticalDirection velocity)

    jumpStartTime_ :: Maybe Seconds
    jumpStartTime_ = case action $ state nikki of
        JumpImpulse t _ _ _ -> Just t
        Airborne ji -> if aHeld then jumpStartTime ji else Nothing
        WallSlide ji _ -> if aHeld then jumpStartTime ji else Nothing
        x -> Nothing

    angleDirection :: Angle -> Maybe HorizontalDirection
    angleDirection angle =
        if abs angle > deg2rad 10 then
            Just $ if angle > 0 then HRight else HLeft
          else
            Nothing

    jumpImpulseDirection angle = fromMaybe oldDirection
        (buttonDirection <|> angleDirection angle)

    clouds (Vector x y) direction = case action $ state nikki of
        WallSlide _ (a : r) ->
            if now - creationTime a > cloudCreationTime then
                newCloud direction : filtered
              else
                filtered
          where
            filtered = filter (\ c -> now - creationTime c < 4 * cloudCreationTime) (a : r)
        x -> [newCloud direction]
      where
        newCloud HLeft = Cloud now (Position x y +~ Position (- fromUber (13 / 2)) (fromUber (24 / 2)) +~ cloudRenderCorrection)
        newCloud HRight = Cloud now (Position x y +~ Position (fromUber (13 / 2)) (fromUber (24 / 2)) +~ cloudRenderCorrection)
        cloudRenderCorrection = Position (- fromUber (5 / 2)) (- fromUber (5 / 2))


-- | updates the possible jumping angle from the contacts
getContactNormals :: Contacts -> [Angle]
getContactNormals = map (foldAngle . toUpAngle . collisionNormal) . nikkiContacts

-- | calculates the angle a possible jump is to be performed in
jumpAngle :: [Angle] -> Maybe Angle
jumpAngle angles =
    let relevantAngles = filter (\ x -> abs x <= 0.5 * pi) $ sortBy (withView abs compare) angles
    in case relevantAngles of
        [] -> Nothing
        list@(a : _) ->
            if any (< 0) list && any (> 0) list then
                -- if nikki's contacts are on both sides of pointing up
                Just 0
              else
                Just a

updateStartTime :: Seconds -> State -> Nikki -> Nikki
updateStartTime now oldState nikki =
    if sameState oldState (state nikki) then
        nikki
      else
        nikki{startTime = now}

sameState a b =
    toActionNumber (action a) == toActionNumber (action b)
    && direction a == direction b
