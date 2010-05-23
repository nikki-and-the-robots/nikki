
module Editor.Binary where


import Data.Binary

import Control.Applicative ((<$>), (<*>))

import Base.Sprited
import Editor.Scene


instance (Show sprited, Binary sprited) => Binary (EObject_ sprited) where
    put (ENikki a b) = do
        putWord8 0
        put a
        put b
    put (ETerminal a b c) = do
        putWord8 1
        put a
        put b
        put c
    put (ERobot a b) = do
        putWord8 2
        put a
        put b
    put (ETile a b) = do
        putWord8 3
        put a
        put b

    get = do
        c <- getWord8
        case c of
            0 -> do
                a <- get
                b <- get
                return $ ENikki a b
            1 -> do
                a <- get
                b <- get
                c <- get
                return $ ETerminal a b c
            2 -> do
                a <- get
                b <- get
                return $ ERobot a b
            3 -> do
                a <- get
                b <- get
                return $ ETile a b

instance Binary UnloadedSprited where
    put (UnloadedSprited x y) = do
        put x
        put y
    get = UnloadedSprited <$> get <*> get

instance Binary Name where
    put (Name n) = put n
    get = Name <$> get


