{-# language MultiParamTypeClasses, FunctionalDependencies, NamedFieldPuns,
     ViewPatterns, ExistentialQuantification, DeriveDataTypeable #-}

module Object.Types where

import Utils

import Data.Abelian
import Data.Dynamic
import Data.List
import Data.Indexable (Index)

import Graphics.Qt as Qt

import Physics.Chipmunk hiding (Position, collisionType)

import Base.Events
import Base.Constants
import Base.Pixmap
import Base.Types


-- * Constants

robotFriction :: Double
robotFriction = 1.0


-- * misc

newtype SortId = SortId {getSortId :: FilePath}
  deriving (Show, Read, Eq)


-- * Sort class

-- | Class that every sort of objects has to implement. This is the interface between
-- the game and the implemented objects.
-- Minimal complete definition: 'sortId', 'size', 'sortRender', 'initialize', 'chipmunk', 'render'

class (Show sort, Typeable sort, Show object, Typeable object) =>
    Sort sort object |
        sort -> object, object -> sort where

    sortId :: sort -> SortId
    size :: sort -> Size Double
    objectEditModeMethods :: sort -> Maybe (ObjectEditModeMethods Sort_)
    objectEditModeMethods _ = Nothing
    sortRender :: sort -> Ptr QPainter -> RenderMode -> IO ()
    editorPosition2QtPosition :: sort -> EditorPosition -> Position Double
    editorPosition2QtPosition sort (EditorPosition x y) =
        Position x (y - height)
      where
        Size _ height = size sort

    -- if Nothing is passed as space, this should be an object 
    -- that is not added to the chipmunk space (i.e. background tiles)
    initialize :: sort -> Maybe Space -> EditorPosition -> Maybe String -> IO object

    immutableCopy :: object -> IO object

    chipmunks :: object -> [Chipmunk]

    objectPosition :: object -> IO Vector

    startControl :: object -> object
    startControl = id

    update :: object -> Index -> Seconds -> Contacts -> (Bool, ControlData) -> IO (Scene Object_ -> Scene Object_, object)
    update o i now contacts cd = do
        o' <- updateNoSceneChange o now contacts cd
        return (id, o')
    
    updateNoSceneChange :: object -> Seconds -> Contacts -> (Bool, ControlData) -> IO object
    updateNoSceneChange o _ _ _ = return o

    render :: object -> sort -> Ptr QPainter -> Offset Double -> Seconds -> IO ()


data RenderMode
    = Iconified
    | InScene {
        offset :: Position Double
      }

-- * Sort class wrappers

data Sort_
    = forall sort object .
        (Sort sort object, Show sort, Typeable sort) =>
            Sort_ sort
  deriving Typeable

instance Show Sort_ where
    show (Sort_ s) = "Sort_ (" ++ show s ++ ")"

instance Eq Sort_ where
    a == b = sortId a == sortId b

data Object_
    = forall sort object .
        (Sort sort object,
            Show sort, Typeable sort, 
            Show object, Typeable object) =>
                Object_ sort object
  deriving (Typeable)

instance Show Object_ where
    show (Object_ s o) = "Object_ (" ++ show o ++ ")"

instance Sort Sort_ Object_ where
    sortId (Sort_ s) = sortId s
    size (Sort_ s) = size s
    objectEditModeMethods (Sort_ s) = objectEditModeMethods s
    sortRender (Sort_ s) = sortRender s
    editorPosition2QtPosition (Sort_ s) = editorPosition2QtPosition s
    initialize (Sort_ sort) space editorPosition state =
        Object_ sort <$> initialize sort space editorPosition state
    immutableCopy (Object_ s o) = Object_ s <$> Object.Types.immutableCopy o
    chipmunks (Object_ _ o) = chipmunks o
    objectPosition (Object_ _ o) = objectPosition o
    startControl (Object_ sort o) = Object_ sort $ startControl o
    update (Object_ sort o) i now contacts cd = do
        (f, o') <- update o i now contacts cd
        return (f, Object_ sort o')
    updateNoSceneChange (Object_ sort o) now contacts cd = Object_ sort <$> updateNoSceneChange o now contacts cd
    render = error "Don't use this function, use render_ instead (that't type safe)"

sort_ :: Object_ -> Sort_
sort_ (Object_ sort _) = Sort_ sort

render_ :: Object_ -> Ptr QPainter -> Offset Double -> Seconds -> IO ()
render_ (Object_ sort o) = render o sort

wrapObjectModifier :: Sort s o => (o -> o) -> Object_ -> Object_
wrapObjectModifier f (Object_ s o) =
    case (cast s, cast o) of
        (Just s_, Just o_) -> Object_ s_ (f o_)

-- * Discriminators

isTerminal :: Sort_ -> Bool
isTerminal sort = SortId "terminal" == sortId sort

isRobot :: Sort_ -> Bool
isRobot (sortId -> (SortId s)) = "robots/" `isPrefixOf` s

isNikki :: Sort_ -> Bool
isNikki s = (SortId "nikki" == sortId s)

isTile :: Sort_ -> Bool
isTile (sortId -> (SortId s)) = "tiles/" `isPrefixOf` s

-- * EditorObject

mkEditorObject :: Sort_ -> EditorPosition -> EditorObject Sort_
mkEditorObject sort pos = EditorObject sort pos (mkOEMState sort pos)

modifyOEMState :: (OEMState sort -> OEMState sort) -> EditorObject sort -> EditorObject sort
modifyOEMState f eo =
    case editorOEMState eo of
         Just x -> eo{editorOEMState = Just $ f x}


-- * pickelable object

data PickleObject = PickleObject {
    pickleSortId :: SortId,
    picklePosition :: EditorPosition,
    pickleOEMState :: Maybe String
  }
    deriving (Read, Show)

editorObject2PickleObject :: EditorObject Sort_ -> PickleObject
editorObject2PickleObject (EditorObject sort p oemState) =
    PickleObject (sortId sort) p (fmap pickleOEM oemState)

-- | converts pickled objects to editor objects
-- needs all available sorts
pickleObject2EditorObject :: [Sort_] -> PickleObject -> EditorObject Sort_
pickleObject2EditorObject allSorts (PickleObject id position oemState) =
    EditorObject sort position (fmap (unpickleOEM sort) oemState)
  where
    sort = case filter ((== id) . sortId) allSorts of
        [x] -> x
        [] -> error ("Sort not found: " ++ getSortId id)





renderChipmunk :: Ptr QPainter -> Offset Double -> Pixmap -> Chipmunk -> IO ()
renderChipmunk painter worldOffset p chipmunk = do
    (position, angle) <- getRenderPosition chipmunk
    renderPixmap painter worldOffset position (Just angle) Nothing p


-- * ObjectEditMode

mkOEMState :: Sort_ -> EditorPosition -> Maybe (OEMState Sort_)
mkOEMState sort editorPosition =
    case objectEditModeMethods sort of
        Nothing -> Nothing
        Just oem -> Just $ OEMState oem (oemInitialState oem editorPosition)

enterModeOEM :: EditorScene Sort_ -> OEMState Sort_ -> OEMState Sort_
enterModeOEM scene (OEMState oem state) =
    OEMState oem (oemEnterMode oem scene state)

updateOEM :: EditorScene Sort_ -> Key -> OEMState Sort_ -> OEMState Sort_
updateOEM scene k (OEMState oem state) =
    OEMState oem (oemUpdate oem scene k state)

renderOEM :: Ptr QPainter -> EditorScene Sort_ -> OEMState Sort_ -> IO ()
renderOEM ptr scene (OEMState oem state) =
    oemRender oem ptr scene state

pickleOEM :: OEMState Sort_ -> String
pickleOEM (OEMState _ state) = state

unpickleOEM :: Sort_ -> String -> OEMState Sort_
unpickleOEM sort state =
    case objectEditModeMethods sort of
        Just x -> OEMState x state







