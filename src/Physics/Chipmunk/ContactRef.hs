{-# language NamedFieldPuns #-}

-- | module for the detection and saving of collisions while chipmunk simulation

module Physics.Chipmunk.ContactRef (
    setMyCollisionType,

    ContactRef,
    Callback(..),
    Watcher(..),
    Permeability(..),
    initContactRef,
    resetContactRef,
    readContactRef,
  ) where


import Data.IORef
import Data.StateVar

import Control.Monad.IO.Class

import Physics.Hipmunk as H hiding (Callback)
import Physics.Hipmunk.Callbacks ()

import Utils


-- * collisionTypes

toNumber :: Enum collisionType => collisionType -> CollisionType
toNumber = toEnum . fromEnum

setMyCollisionType :: Enum collisionType => collisionType -> Shape -> IO ()
setMyCollisionType ct s = collisionType s $= toNumber ct


-- * contact ref stuff

data ContactRef x =
    ContactRef {empty :: x, modifier :: IORef x}

instance Show (ContactRef object) where
    show = const "<CollisionRef>"

data Callback collisionType x
    = Callback (Watcher collisionType x) Permeability

data Watcher collisionType x
    = DontWatch collisionType collisionType
    | Watch
        collisionType
        collisionType
        (Shape -> Shape -> x -> IO x)
    | FullWatch
        collisionType
        collisionType
        (Shape -> Shape -> Vector -> x -> x)

collisionShapes :: Watcher ct x -> (ct, ct)
collisionShapes (DontWatch a b) = (a, b)
collisionShapes (Watch a b _) = (a, b)
collisionShapes (FullWatch a b _) = (a, b)

data Permeability
    = Permeable
--     | Semipermeable
    | Solid

isSolidInPresolve :: Permeability -> Bool
isSolidInPresolve Solid = True
-- isSolidInPresolve Semipermeable = True
isSolidInPresolve Permeable = False

-- | needs an empty contacts value
initContactRef :: Enum collisionType =>
     Space -> x -> [Callback collisionType x] -> IO (ContactRef x)
initContactRef space empty callbacks = do
    ref <- newIORef empty
    mapM_ (installCallback ref) callbacks
    return $ ContactRef empty ref
  where
    installCallback ref (Callback watching permeability) = do
        let (a, b) = collisionShapes watching
        addCollisionHandler space (toNumber a) (toNumber b) $ mkHandler ref watching permeability

mkHandler :: IORef x -> Watcher collisionType x -> Permeability -> CollisionHandler
mkHandler ref watcher permeability =
    Handler (mkBegin permeability) (Just $ mkPreSolve ref watcher permeability) Nothing Nothing

{-mkBegin Semipermeable = Just $ do
    (_, movingShape) <- shapes
    velocity@(Vector x y) <- io $ get $ velocity $ body movingShape
    return $ (y >= 0)-}
mkBegin _ = Nothing

mkPreSolve _ (DontWatch _ _) permeability =
    return $ isSolidInPresolve permeability
mkPreSolve ref (Watch _ _ f) permeability = do
    (shapeA, shapeB) <- shapes
    liftIO $ modifyIORefM ref (f shapeA shapeB)
    return $ isSolidInPresolve permeability
mkPreSolve ref (FullWatch _ _ f) permeability = do
    (shapeA, shapeB) <- shapes
    normal_ <- normal
    liftIO $ modifyIORef ref (f shapeA shapeB normal_)
    return $ isSolidInPresolve permeability


-- updating

-- | resets the ContactRef
resetContactRef :: ContactRef x -> IO ()
resetContactRef (ContactRef empty ref) = do
    writeIORef ref empty


-- | returns the actual state of the contacts
readContactRef :: ContactRef x -> IO x
readContactRef (ContactRef empty ref) =
    readIORef ref



