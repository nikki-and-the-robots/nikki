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
    return $ nikki{state = State action (direction $ state nikki) False}
updateState mode now contacts (True, controlData) nikki = do
    velocity_ <- get $ velocity $ body $ chipmunk nikki
    nikkiPos <- getPosition $ chipmunk nikki
    return $ nikki{state = state' nikkiPos velocity_ considerGhostsState'}
  where
    -- function that creates the next state when given the next considerGhosts value.
    state' :: Vector -> Vector -> (Bool -> State)
    state' nikkiPos velocity_ =
        case (willJump, mJumpImpulseData) of
            -- nikki jumps
            (True, Just impulse) -> State
                (JumpImpulse impulse (JumpInformation (Just now) velocity_ buttonDirection))
                (jumpImpulseDirection $ nikkiCollisionAngle impulse)
            -- nikki touches something
            (False, Just c) ->
                case grips nikkiPos contacts of
                    -- nikki grabs something
                    Just HLeft | rightPushed -> State EndGripImpulse HLeft
                    Just HRight | leftPushed -> State EndGripImpulse HRight
                    Just gripDirection -> State Grip gripDirection
                    -- nikki grabs nothing
                    Nothing ->
                        if isLegsCollision c then
                        -- nikki stands on something
                            if nothingHeld then
                                if touchdown then
                                    State Touchdown newDirection
                                else
                                    State (Wait Nothing) newDirection
                            else
                                State (Walk Nothing) newDirection
                        else if isGhostCollision c then
                        -- nikki is a ghost (boo!) (airborne, but can still jump)
                          case buttonDirection of
                          -- no direction -> Wait
                            Nothing -> State
                                (Wait $ Just $ jumpInformation' velocity_)
                                newDirection
                            Just buttonDirection -> State
                                (Walk $ Just $ jumpInformation' velocity_)
                                newDirection
                        else
                        -- something touches the head that causes jumping capability
                            State
                                (WallSlide (jumpInformation' velocity_)
                                    (map nikkiCollisionAngle collisions)
                                    (clouds nikkiPos newDirection))
                                newDirection
            -- nikki cannot jump
            (_, Nothing) ->
                if hasLegsCollisions then
                -- nikki cannot jump, but has legs collisions
                -- the angle is too steep: nikki slides into grip mode (hopefully)
                    State
                        (SlideToGrip (jumpInformation' velocity_))
                        newDirection
                else
                -- nikki touches nothing relevant
                    State (Airborne (jumpInformation' velocity_)) newDirection

    -- if nikki should jump. Jump button is pushed and nikki is not in slideToGrip mode.
    willJump :: Bool
    willJump = aPushed && not (isSlideToGrip $ action $ state nikki)

    -- nikki's new horizontal direction
    newDirection :: HorizontalDirection
    newDirection = case state nikki of
        State Grip direction _ -> swapHorizontalDirection direction
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
    -- if a given head collision should be treated as nikki grabbing something
    isGripCollision c =
        abs (nikkiCollisionAngle c) < gripAngleLimit

    -- if nikki has collisions with the legs
    hasLegsCollisions = not $ null $ filter isLegsCollision $ nikkiCollisions contacts

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
    mJumpImpulseData :: Maybe NikkiCollision
    mJumpImpulseData = jumpImpulseData (state nikki) collisions
    -- all collisions
    collisions :: [NikkiCollision]
    collisions = nikkiCollisions contacts

    -- while nikki is in the air, this describes the state
    jumpInformation' velocity_ =
        JumpInformation jumpStartTime_ velocity_ buttonDirection

    -- when the jump button is held, this saves the time of the jump's start
    jumpStartTime_ :: Maybe Seconds
    jumpStartTime_ =
        if aHeld
        then getJumpInformation (action $ state nikki) >>= jumpStartTime
        else Nothing

    -- | direction when starting a jump
    jumpImpulseDirection angle = fromMaybe oldDirection
        (buttonDirection <|> angleDirection angle)


    -- | There is a state where nikki's jump impulse will be affected by collisions
    -- with the ghost shapes. These jumps are called ghost jumps.
    -- This happens when the following conditions are met:
    --   1. Nikki had leg collisions.
    --   2. Nikki has no leg collisions anymore
    --   3. Nikki still touches something with the ghost shapes (has ghost collisions)
    --   4. Nikki has not performed a (maybe ghost-) jump since the last real leg collision.
    -- A ghost jump's impulse will be calculated in the same way a normal jump's impulse
    -- is. It will however additionally consider all collisions with the ghost shape
    -- like normal collisions.
    -- condition 2. is not encoded in State.considerGhostsState,
    -- but is done in mJumpImpulseData
    considerGhostsState' :: Bool
    considerGhostsState' =
        hasLegsCollisions ||
        (considerGhostsState (state nikki) &&
         hasGhostCollisions &&
         not (isJumpImpulseAction oldAction))

    -- | all collisions with the ghost shapes
    hasGhostCollisions :: Bool
    hasGhostCollisions =
        not $ null $
        filter isGhostCollision $
        nikkiCollisions contacts

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


-- if a given collision is with nikki's head
isHeadCollision (NikkiCollision _ normal NikkiHeadCT) = True
isHeadCollision (NikkiCollision _ normal NikkiLeftPawCT) = True
isHeadCollision _ = False

-- if a given collision is with nikki's legs
isLegsCollision (NikkiCollision _ _ NikkiLegsCT) = True
isLegsCollision _ = False

isGhostCollision :: NikkiCollision -> Bool
isGhostCollision = (NikkiGhostCT ==) . nikkiCollisionType

-- if a given collision angle should lead to nikki standing on feet
isStandingFeetAngle :: Angle -> Bool
isStandingFeetAngle angle =
    abs angle < (deg2rad 90 - footToHeadAngle + deg2rad 1)
                                                -- angle epsilon

-- | returns the horizontal direction of a given angle
angleDirection :: Angle -> Maybe HorizontalDirection
angleDirection angle =
    if abs angle > deg2rad 10
    then Just $ if angle > 0 then HRight else HLeft
    else Nothing

-- | Calculates the angle for possible jump.
-- Considers ghost collisions depending on the arguments
jumpImpulseData :: State -> [NikkiCollision] -> Maybe NikkiCollision
jumpImpulseData state =
    -- sort (more upward first)
    sortBy (withView (abs . nikkiCollisionAngle) compare) >>>
    -- remove angles pointing downward
    filter (\ x -> abs (nikkiCollisionAngle x) <= pi / 2) >>>
    filterGhostCollisions >>>
    filter causingJumps >>>
    -- sorting collisions: legs, ghost, head
    sortLegsCollisions >>>
    listToMaybe
  where
    sortLegsCollisions = sortBy (withView (nikkiCollisionType >>> toNumber) compare)
    toNumber NikkiLegsCT = 1
    toNumber NikkiGhostCT = 2
    toNumber NikkiHeadCT = 3
    toNumber NikkiLeftPawCT = 3

    causingJumps x =
        (not $ isHeadCollision x) ~>
        (isStandingFeetAngle $ nikkiCollisionAngle x)
    filterGhostCollisions :: [NikkiCollision] -> [NikkiCollision]
    -- consider only ghost collisions
    -- that have a so called standing feet angle
    filterGhostCollisions cs =
        if considerGhostsState state &&
           (null $ filter isLegsCollision cs)
        then cs
        else filter (not . isGhostCollision) cs
    listToMaybe [] = Nothing
    listToMaybe list@(a : _) =
        -- just take the best (if there are two than it (hopefully) doesn't matter)
        -- and set the angle to 0 if there are angle greater and smaller than 0.
        Just $ a{nikkiCollisionAngle = angle}
      where
        angle = if any ((< 0) . nikkiCollisionAngle) list &&
                   any ((> 0) . nikkiCollisionAngle) list
                    -- if nikki's contacts are on both sides of pointing up
                then 0
                else (nikkiCollisionAngle a)


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
