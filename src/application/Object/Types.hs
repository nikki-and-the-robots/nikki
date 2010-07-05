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

data EditorPosition = EditorPosition {
    editorX :: Double,
    editorY :: Double
  }
  deriving (Show, Read, Eq)

instance Abelian EditorPosition where
    zero = EditorPosition 0 0
    (EditorPosition a b) +~ (EditorPosition x y) =
        EditorPosition (a + x) (b + y)
    (EditorPosition a b) -~ (EditorPosition x y) =
        EditorPosition (a - x) (b - y)

newtype SortId = SortId {getSortId :: FilePath}
  deriving (Show, Read, Eq)


-- * Sort class

class (Show sort, Typeable sort, Show object, Typeable object) =>
    Sort sort object |
        sort -> object, object -> sort where

    sortId :: sort -> SortId
    size :: sort -> Size Double
    objectEditMode :: sort -> Maybe ObjectEditMode
    objectEditMode _ = Nothing
    sortRender :: sort -> Ptr QPainter -> Offset Double
        -> EditorPosition -> Maybe (Size Double) -> IO ()
    editorPosition2QtPosition :: sort -> EditorPosition -> Position Double
    editorPosition2QtPosition sort (EditorPosition x y) =
        Position x (y - height)
      where
        Size _ height = size sort

    -- if Nothing is passed as space, this should be an object 
    -- that is not added to the chipmunk space (i.e. background tiles)
    initialize :: sort -> Maybe Space -> EditorPosition -> Maybe String -> IO object

    chipmunk :: object -> Chipmunk

    startControl :: object -> object
    startControl = id

    update :: object -> Index -> Seconds -> Contacts -> (Bool, ControlData) -> IO (Scene Object_ -> Scene Object_, object)
    update o i now contacts cd = do
        o' <- updateNoSceneChange o now contacts cd
        return (id, o')
    
    updateNoSceneChange :: object -> Seconds -> Contacts -> (Bool, ControlData) -> IO object
    updateNoSceneChange o _ _ _ = return o

    render :: object -> sort -> Ptr QPainter -> Offset Double -> Seconds -> IO ()


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
  deriving Typeable

instance Show Object_ where
    show (Object_ s o) = "Object_ (" ++ show o ++ ")"

instance Sort Sort_ Object_ where
    sortId (Sort_ s) = sortId s
    size (Sort_ s) = size s
    objectEditMode (Sort_ s) = objectEditMode s
    sortRender (Sort_ s) = sortRender s
    editorPosition2QtPosition (Sort_ s) = editorPosition2QtPosition s
    initialize (Sort_ sort) space editorPosition state =
        Object_ sort <$> initialize sort space editorPosition state
    chipmunk (Object_ _ o) = chipmunk o
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


-- * Editor objects

data EditorObject
    = EditorObject {
        editorSort :: Sort_,
        editorPosition :: EditorPosition,
        editorOEMState :: Maybe OEMState
      }
    | MergedTilesEditorObject {
        editorMergedObjects :: [EditorObject]
      }
  deriving Show

mkEditorObject :: Sort_ -> EditorPosition -> EditorObject
mkEditorObject sort pos = EditorObject sort pos (mkOEMState sort)

modifyOEMState :: (OEMState -> OEMState) -> EditorObject -> EditorObject
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

editorObject2PickleObject :: EditorObject -> PickleObject
editorObject2PickleObject (EditorObject sort p oemState) =
    PickleObject (sortId sort) p (fmap pickleOEM oemState)

-- | converts pickled objects to editor objects
-- needs all available sorts
pickleObject2EditorObject :: [Sort_] -> PickleObject -> EditorObject
pickleObject2EditorObject allSorts (PickleObject id position oemState) =
    EditorObject sort position (fmap (unpickleOEM sort) oemState)
  where
    sort = case filter ((== id) . sortId) allSorts of
        [x] -> x
        [] -> error ("Sort not found: " ++ getSortId id)



sortRenderSinglePixmap :: Sort sort object =>
    Pixmap -> sort -> Ptr QPainter -> Offset Double
    -> EditorPosition -> Maybe (Size Double) -> IO ()
sortRenderSinglePixmap pix sort ptr offset ep scaling = do
    resetMatrix ptr
    translate ptr offset
    let
        (Size width height) = size sort
        (factor, scalingOffset) = case scaling of
            Just x ->
                squeezeScaling x $ size sort
            Nothing -> (1, zero)
        p = editorPosition2QtPosition sort ep +~ scalingOffset

    translate ptr (p +~ fmap fromIntegral (pixmapOffset pix))
    Qt.scale ptr factor factor

    drawPixmap ptr zero (pixmap pix)


renderChipmunk :: Ptr QPainter -> Offset Double -> Pixmap -> Chipmunk -> IO ()
renderChipmunk painter worldOffset p chipmunk = do
    (position, angle) <- getRenderPosition chipmunk
    renderPixmap painter worldOffset position (Just angle) p


-- * ObjectEditMode

data ObjectEditMode
    = ObjectEditMode {
        oemInitialState :: String,
        oemEnterMode :: Dynamic -> String -> String,
        oemUpdate :: Dynamic -> Key -> String -> String,
        oemRender :: Ptr QPainter -> Dynamic -> String -> IO () -- more args
      }

instance Show ObjectEditMode where
    show = const "<ObjectEditMode>"

data OEMState
    = OEMState {
        oem :: ObjectEditMode,
        oemState :: String
      }
  deriving Show

mkOEMState :: Sort_ -> Maybe OEMState
mkOEMState sort =
    case objectEditMode sort of
        Nothing -> Nothing
        Just oem -> Just $ OEMState oem (oemInitialState oem)

enterModeOEM :: Dynamic -> OEMState -> OEMState
enterModeOEM scene (OEMState oem state) =
    OEMState oem (oemEnterMode oem scene state)

updateOEM :: Dynamic -> Key -> OEMState -> OEMState
updateOEM scene k (OEMState oem state) =
    OEMState oem (oemUpdate oem scene k state)

renderOEM :: Ptr QPainter -> Dynamic -> OEMState -> IO ()
renderOEM ptr scene (OEMState oem state) =
    oemRender oem ptr scene state

pickleOEM :: OEMState -> String
pickleOEM (OEMState _ state) = state

unpickleOEM :: Sort_ -> String -> OEMState
unpickleOEM sort state =
    case objectEditMode sort of
        Just x -> OEMState x state







