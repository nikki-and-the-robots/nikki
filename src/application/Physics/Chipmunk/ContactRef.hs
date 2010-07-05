{-# language NamedFieldPuns #-}

-- | module for the detection and saving of collisions while chipmunk simulation

module Physics.Chipmunk.ContactRef (
    setMyCollisionType,

    ContactRef,
    Callback(..),
    Watcher(..),
    Permeability(..),
    initContactRef,
    peekContacts,
  ) where


import Data.IORef

import Physics.Hipmunk hiding (Callback)

import Utils


-- * collisionTypes

toNumber :: Enum collisionType => collisionType -> CollisionType
toNumber = toEnum . fromEnum

setMyCollisionType :: Enum collisionType => Shape -> collisionType -> IO ()
setMyCollisionType s ct = setCollisionType s (toNumber ct)


-- * contact ref stuff

data ContactRef x =
    ContactRef x (IORef (x -> x))

instance Show (ContactRef object) where
    show = const "<CollisionRef>"

data Callback collisionType x
    = Callback (Watcher collisionType x) Permeability

data Watcher collisionType x
    = Watch
        collisionType
        collisionType
        (Shape -> Shape -> x -> x)
--     | forall a . WatchWrite
--         MyCollisionType
--         MyCollisionType
--         (Shape -> Shape -> a)
--         (Indexable object -> Collisions object -> a -> IO (Collisions object))
    | DontWatch collisionType collisionType

data Permeability = Permeable | Solid

isSolid :: Permeability -> Bool
isSolid Solid = True
isSolid _ = False

-- | needs an empty contacts value
initContactRef :: Enum collisionType =>
     Space -> x -> [Callback collisionType x] -> IO (ContactRef x)
initContactRef space empty callbacks = do
    ref <- newIORef id
    mapM_ (installCallback ref) callbacks
    return $ ContactRef empty ref
  where
    installCallback ref (Callback (Watch a b f) permeability) = do
        addCallback space (toNumber a, toNumber b) $
            Basic $ \ shapeA shapeB -> do
                modifyIORef ref (>>> f shapeA shapeB)
                return (isSolid permeability)
    installCallback _ref (Callback (DontWatch a b) permeability) = do
        addCallback space (toNumber a, toNumber b) $ Basic $ \ _ _ ->
            return $ isSolid permeability





-- initRef :: Space -> Callback o -> IO (Maybe (CollisionRef o))
-- initRef space (Callback (Watch act bct setter) permeability) = do
--     ref <- newIORef Nothing
--     addMyCallback space (act, bct) $
--         Basic $ \ a b -> do
--             writeIORef ref (Just ())
--             return (isSolid permeability)
--     return $ Just $ CollisionRef ref
--         (\ objects collisions () -> return $ setter collisions)
-- initRef space (Callback (DontWatch act bct) permeability) = do
--     addMyCallback space (act, bct) $ Basic $ \ _ _ ->
--         return $ isSolid permeability
--     return Nothing
-- initRef space (Callback (WatchWrite act bct writer setter) permeability) = do
--     ref <- newIORef Nothing
--     addMyCallback space (act, bct) $
--         Basic $ \ a b -> do
--             writeIORef ref (Just (writer a b))
--             return (isSolid permeability)
--     return $ Just $ CollisionRef ref setter


-- updating

-- | updates the contacts and resets the contactRef
peekContacts :: ContactRef x -> IO x
peekContacts (ContactRef empty ref) = do
    f <- readIORef ref
    writeIORef ref id
    return $ f empty
-- updateCollisions objects cs@Collisions{watched} =
--     chainAppM (updateCollisionRef objects) watched (mkEmpty cs)

-- updateCollisionRef :: Indexable o -> CollisionRef o -> Collisions o -> IO (Collisions o)
-- updateCollisionRef objects (CollisionRef ref setter) collisions = do
--     hasContact <- readIORef ref
--     writeIORef ref Nothing
--     case hasContact of
--         Nothing -> return collisions
--         (Just x) -> setter objects collisions x


-- * terminals


-- external getters


