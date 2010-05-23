{-# language NamedFieldPuns, ExistentialQuantification #-}

-- | module for the detection and saving of collisions while chipmunk simulation

module Objects.Collisions (
    Collisions,

    toCollisionType,

    nikkiTouchesGround,
    nikkiTouchesLaser,
    nikkiTouchesTerminal,
    nikkiTouchesMilkMachine,
    whichTerminalCollides,

    emptyCollisions,

    initCollisions,

    updateCollisions,
  ) where

import Utils

import Data.IORef
import Data.Map (Map, empty, insert, elems, toList)
import qualified Data.Map as Map
import qualified Data.Indexable as I
import Data.Indexable hiding (length, toList, findIndices, fromList, empty)
import Data.Maybe

import Physics.Chipmunk hiding (Callback)

import Objects.Types


data Collisions
    = Collisions {
        watched :: ![CollisionRef],
        nikkiTouchesGround :: !Bool,
        nikkiTouchesLaser :: !Bool,
        nikkiTouchesMilkMachine :: !Bool,
        terminals :: !(Map Index Bool)
      }
  deriving Show

data CollisionRef =
    forall a . CollisionRef
        (IORef (Maybe a))
        (Indexable Object -> Collisions -> a -> IO Collisions)

instance Show CollisionRef where
    show = const "<CollisionRef>"


-- * constructors

-- empty in the sense that nothing collides
emptyCollisions :: Collisions
emptyCollisions = Collisions [] False False False empty

mkEmpty :: Collisions -> Collisions
mkEmpty (Collisions watched _ _ _ terminals) =
    Collisions watched False False False (Map.map (const False) terminals)


-- * setter (boolean to True)

setNikkiTouchesGround :: Collisions -> Collisions
setNikkiTouchesGround c = c{nikkiTouchesGround = True}

setNikkiTouchesLaser :: Collisions -> Collisions
setNikkiTouchesLaser c = c{nikkiTouchesLaser = True}

setNikkiTouchesMilkMachine :: Collisions -> Collisions
setNikkiTouchesMilkMachine c = c{nikkiTouchesMilkMachine = True}

-- * conversions

toCollisionType :: (Show a, Show b) => Object_ a b -> MyCollisionType
toCollisionType Tile{}          = TileCT
toCollisionType MergedTile{}    = TileCT
toCollisionType Terminal{}      = ActionCT
toCollisionType Robot{}         = RobotCT
toCollisionType MilkMachine{}   = MilkMachineCT

toCollisionType x               = es "toCollisionType" x

-- initialisation

data Callback
    = Callback
        Watcher
        Permeability

data Watcher
    = Watch
        MyCollisionType
        MyCollisionType
        (Collisions -> Collisions)
    | forall a . WatchWrite
        MyCollisionType
        MyCollisionType
        (Shape -> Shape -> a)
        (Indexable Object -> Collisions -> a -> IO Collisions)
    | DontWatch MyCollisionType MyCollisionType

data Permeability = Permeable | Solid

isSolid :: Permeability -> Bool
isSolid Solid = True
isSolid _ = False

initCollisions :: Space -> Indexable Object -> Collisions -> IO Collisions
initCollisions space objects cs = do
    refs <- mapM (initRef space) (watchedCollisions objects)
    return $ mkEmpty $ cs{watched = catMaybes refs}


watchedCollisions :: Indexable Object -> [Callback]
watchedCollisions objects = [
    Callback (Watch TileCT NikkiFeetCT setNikkiTouchesGround) Solid,
    Callback (Watch RobotCT NikkiFeetCT setNikkiTouchesGround) Solid,

    Callback (WatchWrite ActionCT NikkiBodyCT whichTerminal activateTerminal) Permeable,
    Callback (DontWatch ActionCT NikkiFeetCT) Permeable,
    Callback (DontWatch RobotCT ActionCT) Permeable,
    Callback (DontWatch TileCT ActionCT) Permeable,

    Callback (Watch NikkiBodyCT LaserCT setNikkiTouchesLaser) Permeable,
    Callback (Watch NikkiFeetCT LaserCT setNikkiTouchesLaser) Permeable,
    Callback (DontWatch RobotCT LaserCT) Permeable,

    Callback (Watch NikkiBodyCT MilkMachineCT setNikkiTouchesMilkMachine) Permeable,
    Callback (DontWatch NikkiFeetCT MilkMachineCT) Permeable,
    Callback (DontWatch RobotCT MilkMachineCT) Permeable
  ]



initRef :: Space -> Callback -> IO (Maybe CollisionRef)
initRef space (Callback (Watch act bct setter) permeability) = do
    ref <- newIORef Nothing
    addMyCallback space (act, bct) $
        Basic $ \ a b -> do
            writeIORef ref (Just ())
            return (isSolid permeability)
    return $ Just $ CollisionRef ref
        (\ objects collisions () -> return $ setter collisions)
initRef space (Callback (DontWatch act bct) permeability) = do
    addMyCallback space (act, bct) $ Basic $ \ _ _ ->
        return $ isSolid permeability
    return Nothing
initRef space (Callback (WatchWrite act bct writer setter) permeability) = do
    ref <- newIORef Nothing
    addMyCallback space (act, bct) $
        Basic $ \ a b -> do
            writeIORef ref (Just (writer a b))
            return (isSolid permeability)
    return $ Just $ CollisionRef ref setter


-- updating

updateCollisions :: Indexable Object -> Collisions -> IO Collisions
updateCollisions objects cs@Collisions{watched} =
    chainAppM (updateCollisionRef objects) watched (mkEmpty cs)

updateCollisionRef :: Indexable Object -> CollisionRef -> Collisions -> IO Collisions
updateCollisionRef objects (CollisionRef ref setter) collisions = do
    hasContact <- readIORef ref
    writeIORef ref Nothing
    case hasContact of
        Nothing -> return collisions
        (Just x) -> setter objects collisions x


-- * terminals


-- external getters

nikkiTouchesTerminal :: Collisions -> Bool
nikkiTouchesTerminal = or . elems . terminals

whichTerminalCollides :: Collisions -> Index
whichTerminalCollides collisions =
    let colls = filter snd $ toList $ terminals collisions
    in case colls of
        (a : r) -> fst a


-- collision setters

whichTerminal :: Shape -> Shape -> Shape
whichTerminal terminalShape _ = terminalShape

activateTerminal :: Indexable Object -> Collisions -> Shape -> IO Collisions
activateTerminal objects collisions terminalShape = do
    let terminalBody = getBody terminalShape
    let terminal = single "activatTerminal" $ I.findIndices pred objects
        pred object = isTerminal object && (terminalBody == body (chipmunk object))
    return $ setTerminalActive collisions terminal

setTerminalActive :: Collisions -> Index -> Collisions
setTerminalActive collisions@Collisions{terminals} i =
    collisions{terminals = insert i True terminals}


