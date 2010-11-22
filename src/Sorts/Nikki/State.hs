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
import Sorts.Nikki.Initialisation


updateState :: Mode -> Seconds -> Contacts -> (Bool, ControlData) -> Nikki -> IO Nikki
updateState mode _ _ (False, _) nikki = do
    let action = case mode of
            TerminalMode{} -> UsingTerminal
            RobotMode{} -> UsingTerminal
            (LevelFinished _ result) -> NikkiLevelFinished result
    return $ nikki{state = State action (direction $ state nikki)}
updateState mode now contacts (True, controlData) nikki = do
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
            (False, Just _) ->
            -- nikki touches something
                case grips nikkiPos contacts of
                    -- nikki grabs something
                    Just HLeft | rightPushed -> State EndGripImpulse HLeft
                    Just HRight | leftPushed -> State EndGripImpulse HRight
                    Just gripDirection -> State Grip gripDirection
                    -- nikki grabs nothing
                    Nothing ->
                        if hasLegsCollisions then
                        -- something touches the feet
                            if standsOnFeet then
                            -- nikki stands on feet (the angle is not too steep)
                                if nothingHeld then
                                    if touchdown then
                                        State Touchdown newDirection
                                    else
                                        State Wait newDirection
                                else
                                    State Walk newDirection
                            else
                            -- the angle is too steep: nikki slides into grip mode (hopefully)
                                State
                                    (SlideToGrip (jumpInformation' velocity_))
                                    newDirection
                        else
                        -- something touches the head
                            State
                                (WallSlide (jumpInformation' velocity_)
                                    (map snd contactNormals)
                                    (clouds nikkiPos newDirection))
                                newDirection
            (_, Nothing) ->
            -- nikki touches nothing, is airborne
                State (Airborne (jumpInformation' velocity_)) newDirection

    -- if nikki should jump. Jump button is pushed and nikki is not in slideToGrip mode.
    willJump :: Bool
    willJump = aPushed && not (isSlideToGrip (state nikki))

    -- nikki's new horizontal direction
    newDirection :: HorizontalDirection
    newDirection = case state nikki of
        State Grip direction -> swapHorizontalDirection direction
        _ -> fromMaybe oldDirection buttonDirection

    -- returns if nikki grabs something (and if yes, which direction)
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
    -- if a given collision is with nikki's head
    isHeadCollision (NikkiCollision _ normal NikkiHeadCT) = True
    isHeadCollision (NikkiCollision _ normal NikkiLeftPawCT) = True
    isHeadCollision _ = False
    -- if a given head collision should be treated as nikki grabbing something
    isGripCollision c =
        abs (foldAngle $ toUpAngle $ nikkiCollisionNormal c) < gripAngleLimit

    -- if nikki should be considered standing on feet.
    standsOnFeet = hasStandingFeetCollisions ||
        hasCombinedStandingFeetNormal

    -- if nikki has collisions with the legs
    hasLegsCollisions = not (null legsCollisions)
    -- list of leg collisions
    legsCollisions = filter isLegsCollision $ nikkiCollisions contacts
    -- if a given collision is with nikki's legs
    isLegsCollision (NikkiCollision _ _ NikkiLegsCT) = True
    isLegsCollision _ = False

    -- if one of the leg collisions has the right angle to lead to nikki standing on feet
    hasStandingFeetCollisions =
        any (nikkiCollisionNormal >>> isStandingFeetNormal) legsCollisions
    -- if a given collision normal should lead to nikki standing on feet
    isStandingFeetNormal normal =
        isStandingFeetAngle $ foldAngle $ toUpAngle normal
    -- if a given collision angle should lead to nikki standing on feet
    isStandingFeetAngle angle =
        abs angle < (deg2rad 90 - footToHeadAngle + deg2rad 1)
                                                    -- angle epsilon

    -- If the collisions together result in a contactAngle that allows jumping.
    -- (There might be two (or more) angles, that -- if considered individually -- would
    -- lead to nikki not standing on feet (and sliding to grip mode), but together
    -- should allow jumping, cause they have differing signs.)
    hasCombinedStandingFeetNormal :: Bool
    hasCombinedStandingFeetNormal = case mContactAngle of
        Nothing -> False
        Just (_, contactAngle) -> isStandingFeetAngle contactAngle

    -- if nikki should be in touchdown state (as alternative to Wait)
    touchdown :: Bool
    touchdown =
        isAirborneAction oldAction ||
        (isTouchdownAction oldAction &&
         stateTime < touchdownDuration)

    -- time nikki was already in the old state
    stateTime = now - startTime nikki

    -- Action of nikkis last state
    oldAction = action $ state nikki

    -- button events
    aPushed = Press AButton `elem` pressed controlData
    aHeld = AButton `member` held controlData
    rightPushed = Press RightButton `elem` pressed controlData
    leftPushed = Press LeftButton `elem` pressed controlData
    rightHeld = RightButton `member` held controlData
    leftHeld = LeftButton `member` held controlData
    nothingHeld = not (rightHeld `xor` leftHeld)

    -- nikkis previous horizontal direction
    oldDirection :: HorizontalDirection
    oldDirection = direction $ state nikki
    -- the direction indicated by the buttons (if any)
    buttonDirection :: Maybe HorizontalDirection
    buttonDirection =
        if nothingHeld then Nothing else
        if leftHeld then Just HLeft else
        Just HRight

    -- the contact angle that should be used for jumping (if there are collisions)
    mContactAngle :: Maybe (Shape, Angle)
    mContactAngle = jumpAngle contactNormals
    -- all collision normals (and their corresponding shapes)
    contactNormals :: [(Shape, Angle)]
    contactNormals = getContactNormals contacts

    -- while nikki is in the air, this describes the state
    jumpInformation' velocity_ =
        JumpInformation jumpStartTime_ velocity_ buttonDirection

    -- when the jump button is held, this saves the time of the jump's start
    jumpStartTime_ :: Maybe Seconds
    jumpStartTime_ = case action $ state nikki of
        JumpImpulse t _ _ _ _ -> Just t
        x | aHeld -> jumpStartTime =<< getJumpInformation x
        x -> Nothing

    -- | direction when starting a jump
    jumpImpulseDirection angle = fromMaybe oldDirection
        (buttonDirection <|> angleDirection angle)

    -- | create nikki's dust
    clouds :: Vector -> HorizontalDirection -> [Cloud]
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

-- | returns the horizontal direction of a given angle
angleDirection :: Angle -> Maybe HorizontalDirection
angleDirection angle =
    if abs angle > deg2rad 10
    then Just $ if angle > 0 then HRight else HLeft
    else Nothing

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

-- updates the start time of nikki if applicable
updateStartTime :: Seconds -> State -> Nikki -> Nikki
updateStartTime now oldState nikki =
    if sameState oldState (state nikki) then
        nikki
      else
        nikki{startTime = now}

-- returns if two given state have the same type of action
sameState :: State -> State -> Bool
sameState a b =
    toActionNumber (action a) == toActionNumber (action b)
    && direction a == direction b
