{-# language ViewPatterns, NamedFieldPuns #-}

module Sorts.Nikki.State where


import Prelude hiding (lookup)

import Data.List (sortBy)
import Data.Maybe
import Data.Directions
import qualified Data.Strict as Strict
import Data.Convertable

import Control.Arrow
import Control.Applicative ((<|>))

import qualified Physics.Chipmunk as CM
import Physics.Chipmunk hiding (position, Position)

import Utils

import Base

import Sorts.Nikki.Types
import Sorts.Nikki.Configuration
import Sorts.Nikki.Initialisation
import Sorts.Nikki.Dust


updateState :: Controls -> Mode -> Seconds -> Contacts -> (Bool, ControlData) -> Nikki -> IO Nikki
updateState config mode now _ (False, _) nikki = do
    let action = case mode of
            TerminalMode{} -> UsingTerminal
            RobotMode{} -> UsingTerminal
            (LevelFinished _ result) -> NikkiLevelFinished result
        jumpInformation' = jumpInformation $ state nikki
        State{direction} = state nikki
        newState = State action direction 0 jumpInformation' False
    addDustClouds now nikki{state = newState}
updateState config mode now contacts (True, controlData) nikki = do
    velocity_ <- get $ velocity $ body $ chipmunk nikki
    nikkiPos <- getPosition $ chipmunk nikki
    let newState_ = newState config now contacts controlData nikki nikkiPos velocity_
    addDustClouds now nikki{state = newState_}

newState :: Controls -> Seconds -> Contacts -> ControlData
    -> Nikki -> CM.Position -> Velocity -> State
newState config now contacts controlData nikki nikkiPos velocity =
    mkNewState considerGhostsState' -- (dustClouds $ state nikki)
  where
    -- function that creates the next state when given the next considerGhosts value.
--     mkNewState :: (Bool -> [DustCloud] -> State)
    mkNewState :: Bool -> State
    mkNewState =
      case (willJump, mJumpImpulseData) of
        -- nikki jumps
        (True, Just impulse) ->
          let specialJumpInformation =
                JumpInformation (Strict.Just now) (Strict.Just angle) velocity buttonDirection
              angle = nikkiCollisionAngle impulse
          in State
               (JumpImpulse impulse)
               (jumpImpulseDirection $ nikkiCollisionAngle impulse)
               airborneFeetVelocity
               specialJumpInformation
        -- nikki touches something
        (False, Just c) ->
          if isLegsCollision c then
          -- nikki stands on something
                if nothingHeld then
                    State (Wait False) newDirection 0 jumpInformation'
                  else
                    State (Walk afterAirborne False) newDirection walkingFeetVelocity jumpInformation'
            else case grips of
                -- nikki grabs something
                Just HLeft | rightPressed -> State GripImpulse HLeft 0 jumpInformation'
                Just HRight | leftPressed -> State GripImpulse HRight 0 jumpInformation'
                Just gripDirection -> State Grip gripDirection 0 jumpInformation'
                -- nikki grabs nothing
                Nothing ->
                    if isGhostCollision c then
                    -- nikki is a ghost (boo!) (airborne, but can still jump)
                    case buttonDirection of
                        -- no direction -> Wait
                        Strict.Nothing -> State (Wait True) newDirection
                                      airborneFeetVelocity jumpInformation'
                        Strict.Just buttonDirection -> State
                            (Walk afterAirborne True)
                            newDirection
                            airborneFeetVelocity
                            jumpInformation'
                      else
                        -- something touches the head that causes jumping capability
                        State
                            (wallSlide (map nikkiCollisionAngle collisions))
                            (wallSlideDirection $ nikkiCollisionAngle c)
                            airborneFeetVelocity
                            jumpInformation'
        -- nikki cannot jump
        (_, Nothing) -> case legsCollisions of
            (coll : _) ->
            -- nikki cannot jump, but has legs collisions
            -- the angle is too steep: nikki slides into grip mode (hopefully)
                let wallDirection = swapHorizontalDirection $
                        angleDirection $ nikkiCollisionAngle coll
                in State (SlideToGrip wallDirection)
                    newDirection airborneFeetVelocity jumpInformation'
            _ ->
                -- nikki touches nothing relevant
                State Airborne newDirection airborneFeetVelocity jumpInformation'

    -- Action of nikkis last state
    oldAction = action $ state nikki
    -- nikkis previous horizontal direction
    oldDirection :: HorizontalDirection
    oldDirection = direction $ state nikki
    -- previous feet velocity
    oldFeetVelocity = feetVelocity $ state nikki

    -- velocity of nikki's feet when airborne
    airborneFeetVelocity =
        if xVel > 0
        then max (- maximumWalkingVelocity) (- xVel)
        else min maximumWalkingVelocity (- xVel)
      where
        xVel = vectorX velocity

    -- Velocity of nikki's feet when walking.
    -- If no arrow key is pressed, the feet velocity stops immediately (is set to 0).
    -- If we are for some reason in the situation (for example after a jump),
    -- that the oldFeetVelocity and the wanted feetVelocity have different signs,
    -- we should set the feetVelocity to zero also, to ensure a consistent feel.
    walkingFeetVelocity = case newDirection of
        HLeft -> min
            maximumWalkingVelocity
            (oldFeetVelocityOrZero + feetAcceleration)
          where
            oldFeetVelocityOrZero = max 0 oldFeetVelocity
        HRight -> max
            (- maximumWalkingVelocity)
            (oldFeetVelocityOrZero - feetAcceleration)
          where
            oldFeetVelocityOrZero = min 0 oldFeetVelocity

    -- The acceleration that can be applied to the feet's surface velocity
    -- when walking. (per stepQuantum)
    feetAcceleration :: CpFloat
    feetAcceleration = (maximumWalkingVelocity / accelerationTime) * updateStepQuantum


    -- if the actual action came after being airborne
    -- (assuming the actual action is Walk)
    afterAirborne =
      case oldAction of
        Walk{afterAirborne} -> afterAirborne
        Airborne{} -> True
        _ -> False

    -- if nikki should jump. Jump button is pushed and nikki is not in slideToGrip mode.
    willJump :: Bool
    willJump = jumpButtonPushed &&
        not (isSlideToGripAction oldAction)

    -- nikki's new horizontal direction
    newDirection :: HorizontalDirection
    newDirection = Strict.fromMaybe oldDirection buttonDirection

    -- returns if nikki grabs something (and if yes, which direction)
    grips :: Maybe HorizontalDirection
    grips =
        (nikkiCollisions >>>
         filter isHeadCollision >>>
         filter isGripCollision >>>
         toGripCollision) contacts
      where
        toGripCollision [] = Nothing
        toGripCollision gripCollisions = Just $
            if any ((NikkiLeftPawCT ==) . nikkiCollisionType) gripCollisions
            then HLeft else HRight
    -- if a given head collision should be treated as nikki grabbing something
    isGripCollision c =
        abs (nikkiCollisionAngle c) < gripAngleLimit

    -- collisions with the legs
    legsCollisions = filter isLegsCollision $ nikkiCollisions contacts

    hasLegsCollisions = not $ null $ legsCollisions

    -- button events
    jumpButtonPushed = isGameJumpPressed config controlData
    jumpButtonHeld = isGameJumpHeld config controlData
    rightPressed = isGameRightPressed config controlData
    leftPressed = isGameLeftPressed config controlData
    rightHeld = isGameRightHeld config controlData
    leftHeld = isGameLeftHeld config controlData
    nothingHeld = not (rightHeld `xor` leftHeld)

    -- the direction indicated by the buttons (if any)
    buttonDirection :: Strict.Maybe HorizontalDirection
    buttonDirection =
        if nothingHeld then Strict.Nothing else
        if leftHeld then Strict.Just HLeft else
        Strict.Just HRight

    -- the contact angle that should be used for jumping (if there are collisions)
    mJumpImpulseData :: Maybe NikkiCollision
    mJumpImpulseData = jumpCollision considerGhostsState' collisions
    -- all collisions
    collisions :: [NikkiCollision]
    collisions = nikkiCollisions contacts

    -- while nikki is in the air, this describes the state
    jumpInformation' =
        JumpInformation jumpStartTime_ jumpCollisionAngle_ velocity buttonDirection

    -- when the jump button is held, this saves the time of the jump's start
    jumpStartTime_ :: Strict.Maybe Seconds
    jumpCollisionAngle_ :: Strict.Maybe Angle
    (jumpStartTime_, jumpCollisionAngle_) =
        if jumpButtonHeld
        then (jumpStartTime ji, jumpCollisionAngle ji)
        else (Strict.Nothing, Strict.Nothing)
      where
        ji = jumpInformation $ state nikki

    -- | direction when starting a jump
    jumpImpulseDirection angle =
        fromMaybe oldDirection
            (convert buttonDirection <|> angleMDirection angle)

    -- | direction when wallsliding
    wallSlideDirection angle =
        fromMaybe oldDirection
            (fmap swapHorizontalDirection $ angleMDirection angle)

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
angleMDirection :: Angle -> Maybe HorizontalDirection
angleMDirection angle =
    if abs angle > deg2rad 10
    then Just $ angleDirection angle
    else Nothing

angleDirection :: Angle -> HorizontalDirection
angleDirection angle = if angle > 0 then HRight else HLeft

-- | Calculates the collision causing possible jump.
-- Considers ghost collisions depending on the arguments.
-- Might create an artificial collision if there two or more collisions
-- with angles with opposite signs.
jumpCollision :: Bool -> [NikkiCollision] -> Maybe NikkiCollision
jumpCollision _ [] = Nothing -- provided for clarity (and efficiency?)
jumpCollision considerGhostsState collisions =
  (
    filter (not . isDownward) >>>
    filterGhostCollisions >>>
    sortLegsCollisions >>>
    sortByAngle >>>
    newSpreadCollisions >>>
    addEmergencyJumpCollision >>>
    filter causingJumps >>>
    listToMaybe
  ) collisions -- not best point-free-style, cause addEmergencyJumpCollision needs the
               -- original list of collisions.
  where

    -- | remove angles pointing downward
    isDownward c = abs (nikkiCollisionAngle c) > (pi / 2 + deg2rad 3.5)

    -- | consider only ghost collisions
    -- that have a so called standing feet angle
    filterGhostCollisions :: [NikkiCollision] -> [NikkiCollision]
    filterGhostCollisions cs =
        if considerGhostsState &&
           (null $ filter isLegsCollision cs)
        then cs
        else filter (not . isGhostCollision) cs

    -- | sorting collisions: legs, ghost, head
    sortLegsCollisions = sortBy (withView (nikkiCollisionType >>> toNumber) compare)
    toNumber NikkiLegsCT = 1
    toNumber NikkiGhostCT = 2
    toNumber NikkiHeadCT = 3
    toNumber NikkiLeftPawCT = 3

    -- | sort (more upward first)
    sortByAngle =
        sortBy (withView (abs . nikkiCollisionAngle) compare)

    -- | If there are collisions with angles with different signs,
    -- we want a new artificial collision with angle 0.
    -- This does not (in no case) consider ghost collisions.
    newSpreadCollisions collisions =
        if any ((< 0) . nikkiCollisionAngle) consideredCollisions &&
           any ((> 0) . nikkiCollisionAngle) consideredCollisions
        then (head consideredCollisions){nikkiCollisionAngle = 0} : collisions
             -- Adds the artificial collision.
             -- Just takes the first collision. The second would probably not be worse, but we have to pick one.
        else collisions
      where
        consideredCollisions = filter (not . isGhostCollision) collisions

    -- | adds a collision in the case that nikki's best collision is a
    -- leg collision that doesn't count as a standing feet collision
    -- AND something else touches nikki's head from above.
    -- This is to prevent Nikki from getting stuck under e.g. boxes
    addEmergencyJumpCollision :: [NikkiCollision] -> [NikkiCollision]
    addEmergencyJumpCollision l@(a : _) =
        if isLegsCollision a &&
           (not $ isStandingFeetAngle $ nikkiCollisionAngle a) &&
           (not $ null downwardHeadCollisions)
        then emergencyJumpCollision : l
        else l
      where
        originalCollisions = collisions
        downwardHeadCollisions = filter isHeadCollision $ filter isDownward originalCollisions
        -- (will only be used if (not $ null downwardHeadCollisions)
        firstDownwardHeadCollision = head downwardHeadCollisions
        emergencyJumpCollision =
            a{nikkiCollisionAngle = newAngle}
        newAngle = mirrorAtHorizon $ nikkiCollisionAngle firstDownwardHeadCollision
        mirrorAtHorizon =
            subtract (pi / 2) >>>
            negate >>>
            (+ pi / 2)
    addEmergencyJumpCollision [] = []

    -- | if a single collision would cause a jump.
    -- Does (of course) not consider spread collisions.
    causingJumps x =
        (not $ isHeadCollision x) ~>
        (isStandingFeetAngle $ nikkiCollisionAngle x)

    -- | putting the head in a Just
    listToMaybe [] = Nothing
    listToMaybe (a : _) = Just a


-- updates the start time of nikki if applicable
updateStartTime :: Seconds -> State -> Nikki -> Nikki
updateStartTime now oldState nikki =
    if sameState oldState (state nikki)
    then nikki
    else nikki{startTime = now}

-- returns if two given state have the same type of action
sameState :: State -> State -> Bool
sameState a b =
    toActionNumber (action a) == toActionNumber (action b)
    && direction a == direction b
