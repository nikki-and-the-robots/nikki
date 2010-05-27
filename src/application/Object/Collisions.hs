{-# language NamedFieldPuns, ExistentialQuantification #-}

-- | module for the detection and saving of collisions while chipmunk simulation

module Object.Collisions (
    Collisions,

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
import Data.Indexable hiding (length, toList, findIndices, fromList, empty)
import Data.Maybe

import Physics.Chipmunk hiding (Callback, position)


data Collisions object
    = Collisions {
        watched :: ![CollisionRef object],
        nikkiTouchesGround :: !Bool,
        nikkiTouchesLaser :: !Bool,
        nikkiTouchesMilkMachine :: !Bool,
        terminals :: !(Map Index Bool)
      }
  deriving Show

data CollisionRef object =
    forall a . CollisionRef
        (IORef (Maybe a))
        (Indexable object -> Collisions object -> a -> IO (Collisions object))

instance Show (CollisionRef object) where
    show = const "<CollisionRef>"


-- * constructors

-- empty in the sense that nothing collides
emptyCollisions :: Collisions object
emptyCollisions = Collisions [] False False False empty

mkEmpty :: Collisions o -> Collisions o
mkEmpty (Collisions watched _ _ _ terminals) =
    Collisions watched False False False (Map.map (const False) terminals)


-- * setter (boolean to True)

setNikkiTouchesGround :: Collisions o -> Collisions o
setNikkiTouchesGround c = c{nikkiTouchesGround = True}

setNikkiTouchesLaser :: Collisions o -> Collisions o
setNikkiTouchesLaser c = c{nikkiTouchesLaser = True}

setNikkiTouchesMilkMachine :: Collisions o -> Collisions o
setNikkiTouchesMilkMachine c = c{nikkiTouchesMilkMachine = True}


-- initialisation

data Callback object
    = Callback
        (Watcher object)
        Permeability

data Watcher object
    = Watch
        MyCollisionType
        MyCollisionType
        (Collisions object -> Collisions object)
    | forall a . WatchWrite
        MyCollisionType
        MyCollisionType
        (Shape -> Shape -> a)
        (Indexable object -> Collisions object -> a -> IO (Collisions object))
    | DontWatch MyCollisionType MyCollisionType

data Permeability = Permeable | Solid

isSolid :: Permeability -> Bool
isSolid Solid = True
isSolid _ = False

initCollisions :: Space -> Indexable o -> Collisions o -> IO (Collisions o)
initCollisions space objects cs = do
    refs <- mapM (initRef space) (watchedCollisions objects)
    return $ mkEmpty $ cs{watched = catMaybes refs}


watchedCollisions :: Indexable o -> [Callback o]
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



initRef :: Space -> Callback o -> IO (Maybe (CollisionRef o))
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

updateCollisions :: Indexable o -> Collisions o -> IO (Collisions o)
updateCollisions objects cs@Collisions{watched} =
    chainAppM (updateCollisionRef objects) watched (mkEmpty cs)

updateCollisionRef :: Indexable o -> CollisionRef o -> Collisions o -> IO (Collisions o)
updateCollisionRef objects (CollisionRef ref setter) collisions = do
    hasContact <- readIORef ref
    writeIORef ref Nothing
    case hasContact of
        Nothing -> return collisions
        (Just x) -> setter objects collisions x


-- * terminals


-- external getters

nikkiTouchesTerminal :: Collisions o -> Bool
nikkiTouchesTerminal = or . elems . terminals

whichTerminalCollides :: Collisions o -> Index
whichTerminalCollides collisions =
    let colls = filter snd $ toList $ terminals collisions
    in case colls of
        (a : r) -> fst a


-- collision setters

whichTerminal :: Shape -> Shape -> Shape
whichTerminal terminalShape _ = terminalShape

activateTerminal :: Indexable object -> Collisions o -> Shape -> IO (Collisions o)
activateTerminal objects collisions terminalShape = do
    e "activateTerminal"
--     let terminalBody = getBody terminalShape
--     let terminal = single "activatTerminal" $ I.findIndices pred objects
--         pred :: object -> Bool
--         pred object = isTerminal (sort_ object) && (terminalBody == body (chipmunk_ object))
--     return $ setTerminalActive collisions terminal

setTerminalActive :: Collisions o -> Index -> Collisions o
setTerminalActive collisions@Collisions{terminals} i =
    collisions{terminals = insert i True terminals}


