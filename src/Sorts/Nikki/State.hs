{-# language ViewPatterns, NamedFieldPuns, ScopedTypeVariables #-}

module Sorts.Nikki.State where


import Prelude hiding (lookup)

import Data.List (sortBy)
import Data.Maybe
import Data.Directions
import qualified Data.Strict as Strict
import Data.Strict (Pair(..))
import Data.Convertable

import Text.Logging

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
        newState = State action direction 0 jumpInformation' Strict.Nothing
    addDustClouds now nikki{state = newState}
updateState config mode now contacts (True, controlData) nikki = do
    velocity_ <- get $ velocity $ body $ chipmunk nikki
    nikkiPos <- getPosition $ chipmunk nikki
    let newState_ = newState config now contacts controlData nikki nikkiPos velocity_
    addDustClouds now nikki{state = newState_}

newState :: Controls -> Seconds -> Contacts -> ControlData
    -> Nikki -> CM.Position -> Velocity -> State
newState config now contacts controlData nikki nikkiPos velocity =
    case (willJump, mJumpImpulseData) of
      -- nikki jumps
      (True, Strict.Just impulse) -> jumpState impulse
      -- nikki touches something
      (False, Strict.Just c) ->
        if isLegsCollision c then
          -- nikki stands on something
          if nothingHeld then
            State (Wait c) newDirection 0 jumpInformation' Strict.Nothing
           else
            State (Walk afterAirborne c) newDirection
              walkingFeetVelocity jumpInformation' Strict.Nothing
          else case grips of
            -- nikki grabs something
            Just HLeft | rightPressed ->
              State GripImpulse HLeft 0 jumpInformation' Strict.Nothing
            Just HRight | leftPressed ->
              State GripImpulse HRight 0 jumpInformation' Strict.Nothing
            Just gripDirection ->
              State Grip gripDirection 0 jumpInformation' Strict.Nothing
            -- nikki grabs nothing
            Nothing ->
              -- something touches the head that causes jumping capability
              State
                (wallSlide collisions c)
                (wallSlideDirection $ nikkiCollisionAngle c)
                airborneFeetVelocity
                jumpInformation'
                Strict.Nothing
      (_, Strict.Nothing) ->
        case ghostTime' of
          (Strict.Just (_ :!: collision)) ->
            -- nikki is a ghost (boo!) (airborne, but can still jump)
            if willJump then
              jumpState collision
             else
              case buttonDirection of
                -- no direction -> Wait
                Strict.Nothing -> State (Wait collision) newDirection
                  airborneFeetVelocity jumpInformation'
                  ghostTime'
                Strict.Just buttonDirection -> State
                  (Walk afterAirborne collision)
                  newDirection
                  airborneFeetVelocity
                  jumpInformation'
                  ghostTime'
          Strict.Nothing ->
            -- nikki cannot jump
            case legsCollisions of
              (coll : _) ->
                -- nikki cannot jump, but has legs collisions
                -- the angle is too steep: nikki slides into grip mode (hopefully)
                let wallDirection = swapHorizontalDirection $
                           angleDirection $ nikkiCollisionAngle coll
                in State (SlideToGrip wallDirection)
                     newDirection airborneFeetVelocity jumpInformation'
                     Strict.Nothing
              _ ->
                -- nikki touches nothing relevant
                State
                  Airborne
                  newDirection
                  airborneFeetVelocity
                  jumpInformation'
                  Strict.Nothing

  where

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
    mJumpImpulseData :: Strict.Maybe NikkiCollision
    mJumpImpulseData =
        jumpCollision collisions
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

    -- | PRE: null collisions
    ghostTime' :: Strict.Maybe (Pair Seconds NikkiCollision)
    ghostTime' = case oldGhostTime of
        (Strict.Just (t :!: _)) ->
            if now - ghostDuration < t then oldGhostTime else Strict.Nothing
        Strict.Nothing -> case oldAction of
            (Wait collision) -> Strict.Just (now :!: collision)
            (Walk _ collision) -> Strict.Just (now :!: collision)
            (WallSlide_ _ collision) -> Strict.Just (now :!: collision)
            _ -> Strict.Nothing
    oldGhostTime = ghostTime $ state nikki

    jumpState :: NikkiCollision -> State
    jumpState impulse =
        let specialJumpInformation =
                JumpInformation (Strict.Just now) (Strict.Just angle) velocity buttonDirection
            angle = nikkiCollisionAngle impulse
        in State
            (JumpImpulse impulse)
            (jumpImpulseDirection $ nikkiCollisionAngle impulse)
            airborneFeetVelocity
            specialJumpInformation
            Strict.Nothing


-- if a given collision is with nikki's head
isHeadCollision (NikkiCollision _ normal NikkiHeadCT) = True
isHeadCollision (NikkiCollision _ normal NikkiLeftPawCT) = True
isHeadCollision _ = False

-- if a given collision is with nikki's legs
isLegsCollision (NikkiCollision _ _ NikkiLegsCT) = True
isLegsCollision _ = False

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
jumpCollision :: [NikkiCollision] -> Strict.Maybe NikkiCollision
jumpCollision [] = Strict.Nothing -- provided for clarity (and efficiency?)
jumpCollision collisions =
  (
    filter (not . isDownward) >>>
    sortLegsCollisions >>>
    sortByAngle >>>
    newSpreadCollisions >>>
    addEmergencyJumpCollision >>>
    filter causingJumps >>>
    listToMaybe
  ) collisions -- not best point-free-style, because addEmergencyJumpCollision needs the
               -- original list of collisions.
  where

    -- | remove angles pointing downward
    isDownward c = abs (nikkiCollisionAngle c) > (pi / 2 + deg2rad 3.5)

    -- | sorting collisions: legs, ghost, head
    sortLegsCollisions = sortBy (compare `on` (nikkiCollisionType >>> toNumber))
    toNumber NikkiLegsCT    = 1
    toNumber NikkiGhostCT   = 2
    toNumber NikkiHeadCT    = 3
    toNumber NikkiLeftPawCT = 3

    -- | sort (more upward first)
    sortByAngle =
        sortBy (compare `on` (abs . nikkiCollisionAngle))

    -- | If there are collisions with angles with different signs,
    -- we want a new artificial collision with angle 0.
    -- This does not (in no case) consider ghost collisions.
    newSpreadCollisions collisions =
        if any ((< 0) . nikkiCollisionAngle) collisions &&
           any ((> 0) . nikkiCollisionAngle) collisions
        then (head collisions){nikkiCollisionAngle = 0} : collisions
             -- Adds the artificial collision.
             -- Just takes the first collision. The second would probably not be worse, but we have to pick one.
        else collisions

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
    listToMaybe [] = Strict.Nothing
    listToMaybe (a : _) = Strict.Just a


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
