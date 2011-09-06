{-# language Ã¶ MultiParamTypeClasses, DeriveDataTypeable #-}


import Safe

import Data.Data

import System.Environment

import Utils

import Base

import Object.Types

import qualified Editor.Pickle.Types as NewFormat


main = do
    files <- getArgs
    mapM_ convertFile files

convertFile file = do
    content <- readFile file
    let output = show $ convert $ readNote file content
    print $ length output
    writeFile file output

convert :: OldSaveType -> NewFormat.SaveType
convert = NewFormat.pickle . fmap toDummyObject

type OldSaveType = Grounds PickleObject


-- * legacy types

data PickleObject = PickleObject {
    pickleSortId :: SortId,
    picklePosition :: EditorPosition,
    pickleOEMState :: Maybe String
  }
    deriving (Read, Show)

-- * dummy object

toDummyObject :: PickleObject -> EditorObject Sort_
toDummyObject (PickleObject sortId pos oemState) =
    EditorObject (Sort_ $ dummySort sortId) pos (fmap (OEMState . dummyOEMState) oemState)

data DummySort = DummySort SortId
  deriving (Show, Typeable)

data DummyObject = DummyObject
  deriving (Show, Typeable)

instance Sort DummySort DummyObject where
    sortId (Main.DummySort x) = x

dummySort :: SortId -> DummySort
dummySort = Main.DummySort

data DummyOEMState = DummyOEMState String
  deriving (Typeable, Data)

instance IsOEMState DummyOEMState where
    oemPickle (DummyOEMState x) = x

dummyOEMState :: String -> DummyOEMState
dummyOEMState = DummyOEMState
