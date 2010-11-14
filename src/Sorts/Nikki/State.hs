{-# language ViewPatterns #-}

module Sorts.Nikki.State where


import Prelude hiding (lookup)

import Data.List (sortBy)
import Data.Set (member)
import Data.Abelian
import Data.Maybe

import Control.Arrow

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
        case (willJump, mContactAngle) of
            -- nikki jumps
            (True, Just (shape, contactAngle)) -> State
                (JumpImpulse now shape contactAngle velocity_ buttonDirection)
                (jumpImpulseDirection contactAngle)
            -- nikki touches something
            (False, Just _) ->
                case grips nikkiPos contacts of
                    -- nikki grabs something
                    Just HLeft | rightPushed -> State EndGripImpulse HLeft
                    Just HRight | leftPushed -> State EndGripImpulse HRight
                    Just gripDirection -> State Grip gripDirection
                    -- nikki grabs nothing
                    Nothing ->
                        if hasLegsCollisions then
                        -- something touches the feet
                            if hasStandingFeetCollisions then
                                if nothingHeld then
                                    State Wait newDirection
                                else
                                    State Walk newDirection
                              else
                                State
                                    (SlideToGrip (jumpInformation' velocity_))
                                    newDirection
                          else
                            State (WallSlide (jumpInformation' velocity_)
                                    (map snd contactNormals)
                                    (clouds nikkiPos newDirection))
                                newDirection
            (_, Nothing) ->
                State (Airborne (jumpInformation' velocity_)) newDirection

    willJump :: Bool
    willJump = aPushed && not (isSlideToGrip (state nikki))

    newDirection :: HorizontalDirection
    newDirection = case state nikki of
        State Grip direction -> swapHorizontalDirection direction
        _ -> fromMaybe oldDirection buttonDirection
    -- returns if nikki grabs something
    grips :: CM.Position -> Contacts -> Maybe HorizontalDirection
    grips nikkiPos =
        nikkiCollisions >>>
        filter isHeadCollision >>>
        filter isGripCollision >>>
        inner
      where
        inner [] = Nothing
        inner gripCollisions = Just $
            if any ((NikkiLeftPawCT ==) . nikkiCollisionType) gripCollisions
            then HLeft else HRight
    isHeadCollision (NikkiCollision _ normal NikkiHeadCT) = True
    isHeadCollision (NikkiCollision _ normal NikkiLeftPawCT) = True
    isHeadCollision _ = False
    isGripCollision c =
        abs (foldAngle $ toUpAngle $ nikkiCollisionNormal c) < deg2rad 18


    hasLegsCollisions = not (null legsCollisions)
    legsCollisions = filter isLegsCollision $ nikkiCollisions contacts
    isLegsCollision (NikkiCollision _ _ NikkiLegsCT) = True
    isLegsCollision _ = False

    hasStandingFeetCollisions = any isStandingFeetCollision legsCollisions
    isStandingFeetCollision (NikkiCollision _ normal _) =
        abs (foldAngle $ toUpAngle normal) < deg2rad (90 - 25)

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

    mContactAngle :: Maybe (Shape, Angle)
    mContactAngle = jumpAngle contactNormals
    contactNormals :: [(Shape, Angle)]
    contactNormals = getContactNormals contacts

    jumpInformation' velocity_ =
        JumpInformation jumpStartTime_ velocity_ buttonDirection

    jumpStartTime_ :: Maybe Seconds
    jumpStartTime_ = case action $ state nikki of
        JumpImpulse t _ _ _ _ -> Just t
        Airborne ji -> if aHeld then jumpStartTime ji else Nothing
        WallSlide ji _ _ -> if aHeld then jumpStartTime ji else Nothing
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
        WallSlide _ _ (a : r) ->
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
getContactNormals :: Contacts -> [(Shape, Angle)]
getContactNormals =
    nikkiCollisions >>>
    map (\ nc -> (nikkiCollisionShape nc, foldAngle (toUpAngle (nikkiCollisionNormal nc))))

-- | calculates the angle a possible jump is to be performed in
jumpAngle :: [(Shape, Angle)] -> Maybe (Shape, Angle)
jumpAngle angles =
    let relevantAngles = filter (\ x -> abs (snd x) <= 0.5 * pi) $ sortBy (withView (abs . snd) compare) angles
    in case relevantAngles of
        [] -> Nothing
        list@(a : _) ->
            if any ((< 0) . snd) list && any ((> 0) . snd) list then
                -- if nikki's contacts are on both sides of pointing up
                Just (fst a, 0)
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
