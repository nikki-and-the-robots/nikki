{-# language ViewPatterns, NamedFieldPuns, ScopedTypeVariables #-}

module Sorts.Nikki.State where


import Prelude hiding (lookup)

import Data.List (sortBy)
import Data.Maybe
import Data.Directions
import qualified Data.Strict as Strict
import Data.Convertable
import Data.Initial

import Control.Arrow

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
        newState = State action direction 0 jumpInformation' initial
    addDustClouds now nikki{state = newState}
updateState config mode now contacts (True, controlData) nikki = do
    velocity_ <- get $ velocity $ body $ chipmunk nikki
    nikkiPos <- getPosition $ chipmunk nikki
    let newState_ = nextState config now contacts controlData nikki nikkiPos velocity_
    addDustClouds now nikki{state = newState_}

nextState :: Controls -> Seconds -> Contacts -> ControlData
    -> Nikki -> CM.Position -> Velocity -> State
nextState config now contacts controlData nikki nikkiPos velocity =
    let ghostTimes' = nextGhostTimes now (state nikki)
        grips' :: Maybe HorizontalDirection =
            grips collisions
        action' :: Action = nextAction config (state nikki) controlData nothingHeld
                                buttonDirection contacts ghostTimes' grips'
        jumpInformation' = nextJumpInformation (state nikki)
                            config controlData buttonDirection now velocity action'
        direction' :: HorizontalDirection = nextDirection (state nikki)
                                            buttonDirection action' grips'
        feetVelocity' :: CpFloat = nextFeetVelocity (state nikki)
                                        collisions velocity
                                        action' direction'
    in State action' direction' feetVelocity' jumpInformation' ghostTimes'
  where
    -- all collisions
    collisions :: [NikkiCollision]
    collisions = nikkiCollisions contacts

    rightHeld = isGameRightHeld config controlData
    leftHeld = isGameLeftHeld config controlData
    nothingHeld = not (rightHeld `xor` leftHeld)
    -- the direction indicated by the buttons (if any)
    buttonDirection :: Strict.Maybe HorizontalDirection
    buttonDirection =
        if nothingHeld then Strict.Nothing else
        if leftHeld then Strict.Just HLeft else
        Strict.Just HRight

nextGhostTimes :: Seconds -> State -> GhostTimes
nextGhostTimes now oldState = case oldAction of
    JumpImpulse{} -> initial
    (Wait collision False) -> GhostTimes (Strict.Just (now :!: collision)) Strict.Nothing
    (Walk _ collision False) -> GhostTimes (Strict.Just (now :!: collision)) Strict.Nothing
    (WallSlide_ _ collision) ->
        wallSlideGhostTime ^= (Strict.Just (now :!: collision)) $
            updateStanding oldGhostTimes
    _ -> updateStanding $ updateWallSlide oldGhostTimes
  where
    oldGhostTimes = ghostTimes oldState
    oldAction = action oldState

    updateStanding = standingGhostTime ^: update
    updateWallSlide = wallSlideGhostTime ^: update
    update :: Strict.Maybe (Pair Seconds NikkiCollision) -> Strict.Maybe (Pair Seconds NikkiCollision)
    update oldGhostTime = case oldGhostTime of
        Strict.Nothing -> Strict.Nothing
        Strict.Just (t :!: _) ->
            if now - ghostDuration < t then oldGhostTime else Strict.Nothing

-- returns if nikki grabs something (and if yes, which direction)
grips :: [NikkiCollision] -> Maybe HorizontalDirection
grips =
    filter isHeadCollision >>>
    filter isGripCollision >>>
    toGripCollision
  where
    -- if a given head collision should be treated as nikki grabbing something
    isGripCollision c = abs (nikkiCollisionAngle c) < gripAngleLimit

    toGripCollision [] = Nothing
    toGripCollision gripCollisions = Just $
        if any ((NikkiLeftPawCT ==) . nikkiCollisionType) gripCollisions
        then HLeft else HRight


nextAction config oldState controlData nothingHeld buttonDirection contacts ghostTimes' grips =
    case (willJump, mJumpImpulseData) of
      -- nikki jumps
      (True, Strict.Just impulse) ->
        case (oldAction, ghostTimes' ^. standingGhostTime) of
          (WallSlide_{}, Strict.Just (_ :!: ghostImpulse)) -> JumpImpulse ghostImpulse
          _ -> JumpImpulse impulse
      -- nikki touches something
      (False, Strict.Just c) ->
        if isLegsCollision c then
          -- nikki stands on something
          if nothingHeld
          then Wait c False
          else Walk afterAirborne c False
        else case grips of
            -- nikki grabs something
            Just HLeft  | rightPressed -> GripImpulse
            Just HRight | leftPressed  -> GripImpulse
            Just _ -> Grip
            -- something touches the head that causes jumping capability
            Nothing ->
                wallSlide (nikkiCollisions contacts) c
      (_, Strict.Nothing) ->
        case flattenGhostTime ghostTimes' of
          (Strict.Just (_ :!: collision)) ->
            -- nikki is a ghost (boo!) (airborne, but can still jump)
            if willJump
              then JumpImpulse collision
              else case buttonDirection of
                -- no direction -> Wait
                Strict.Nothing -> Wait collision True
                Strict.Just buttonDirection -> Walk afterAirborne collision True
          Strict.Nothing ->
            -- nikki cannot jump
            case legsCollisions of
              (coll : _) ->
                -- nikki cannot jump, but has legs collisions
                -- the angle is too steep: nikki slides into grip mode (hopefully)
                let wallDirection = swapHorizontalDirection $
                           angleDirection $ nikkiCollisionAngle coll
                in SlideToGrip wallDirection
              -- nikki touches nothing relevant
              _ -> Airborne

  where
    -- Action of nikkis last state
    oldAction = action oldState

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

    -- collisions with the legs
    legsCollisions = filter isLegsCollision $ nikkiCollisions contacts

    -- button events
    jumpButtonPushed = isGameJumpPressed config controlData
    rightPressed = isGameRightPressed config controlData
    leftPressed = isGameLeftPressed config controlData

    -- the contact angle that should be used for jumping (if there are collisions)
    mJumpImpulseData :: Strict.Maybe NikkiCollision
    mJumpImpulseData =
        jumpCollision (nikkiCollisions contacts)

-- while nikki is in the air, this describes the state
nextJumpInformation oldState config controlData buttonDirection now velocity action =
  case action of
    JumpImpulse impulse ->
        JumpInformation
            (Strict.Just now)
            (Strict.Just (nikkiCollisionAngle impulse))
            velocity buttonDirection
    _ -> JumpInformation jumpStartTime_ jumpCollisionAngle_ velocity buttonDirection
  where
    -- when the jump button is held, this saves the time of the jump's start
    jumpStartTime_ :: Strict.Maybe Seconds
    jumpCollisionAngle_ :: Strict.Maybe Angle
    (jumpStartTime_, jumpCollisionAngle_) =
        if jumpButtonHeld
        then (jumpStartTime oldJi, jumpCollisionAngle oldJi)
        else (Strict.Nothing, Strict.Nothing)
      where
        oldJi = jumpInformation oldState
    jumpButtonHeld = isGameJumpHeld config controlData


nextDirection oldState buttonDirection action grips = case grips of
    Just d -> d
    Nothing -> case action of
        JumpImpulse impulse -> jumpImpulseDirection $ nikkiCollisionAngle impulse
        WallSlide_ _ c -> wallSlideDirection $ nikkiCollisionAngle c
        _ -> newDirection
  where

    -- | direction when starting a jump
    jumpImpulseDirection angle =
        fromMaybe oldDirection
            (convert buttonDirection <|> angleMDirection angle)

    -- | direction when wallsliding
    wallSlideDirection angle =
        fromMaybe oldDirection
            (fmap swapHorizontalDirection $ angleMDirection angle)

    -- nikki's new horizontal direction
    newDirection :: HorizontalDirection
    newDirection = Strict.fromMaybe oldDirection buttonDirection
    -- nikkis previous horizontal direction
    oldDirection :: HorizontalDirection
    oldDirection = direction oldState


nextFeetVelocity :: State -> [NikkiCollision] -> Velocity -> Action
    -> HorizontalDirection -> CpFloat
nextFeetVelocity oldState collisions velocity action direction = case action of
    WallSlide_{} -> airborneFeetVelocity
    Walk{} -> walkingFeetVelocity
    _ -> if null collisions
        then airborneFeetVelocity
        else 0
  where
    -- velocity of nikki's feet when airborne
    airborneFeetVelocity =
        if xVel > 0
        then max (- maximumWalkingVelocity) (- xVel)
        else min maximumWalkingVelocity (- xVel)
    xVel = vectorX velocity
    -- Velocity of nikki's feet when walking.
    -- If no arrow key is pressed, the feet velocity stops immediately (is set to 0).
    -- If we are for some reason in the situation (for example after a jump),
    -- that the oldFeetVelocity and the wanted feetVelocity have different signs,
    -- we should set the feetVelocity to zero also, to ensure a consistent feel.
    walkingFeetVelocity = case direction of
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
    -- previous feet velocity
    oldFeetVelocity = feetVelocity oldState


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
    filter (isGhostCollision ~.> isHorizontalCollision) >>>
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

    isGhostCollision c = NikkiGhostCT == nikkiCollisionType c
    isHorizontalCollision c = abs (nikkiCollisionAngle c) =~= (tau / 4)
      where
        a =~= b = abs (a - b) < eps
        eps = deg2rad 3.5

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
