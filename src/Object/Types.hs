{-# language MultiParamTypeClasses, FunctionalDependencies, NamedFieldPuns,
     ViewPatterns, ExistentialQuantification, DeriveDataTypeable #-}

module Object.Types where

import Utils

import Data.Dynamic

import Graphics.Qt as Qt

import Physics.Chipmunk hiding (Position, collisionType)

import Base


-- * misc

type Application = Application_ Sort_

-- * Sort class wrappers

data Sort_
    = forall sort object .
        (Sort sort object, Show sort, Typeable sort) =>
            Sort_ sort
    | DummySort -- used if the wrapper object (Object_) will find the sort.
  deriving Typeable

instance Show Sort_ where
    show (Sort_ s) = "Sort_ (" ++ show s ++ ")"

instance Eq Sort_ where
    a == b = sortId a == sortId b

instance Sort Sort_ Object_ where
    sortId (Sort_ s) = sortId s
    freeSort (Sort_ s) = freeSort s
    size (Sort_ s) = size s
    objectEditMode (Sort_ s) = objectEditMode s
    renderIconified (Sort_ s) = renderIconified s
    renderEditorObject ptr offset editorObject =
        case editorSort editorObject of
            (Sort_ innerSort) ->
                renderEditorObject ptr offset editorObject{editorSort = innerSort}
    initialize (Sort_ sort) space editorPosition state =
        Object_ sort <$> initialize sort space editorPosition state
    immutableCopy (Object_ s o) = Object_ s <$> Base.immutableCopy o
    chipmunks (Object_ _ o) = chipmunks o
    getControlledChipmunk scene (Object_ _ o) = getControlledChipmunk scene o
    startControl now (Object_ sort o) = Object_ sort $ startControl now o
    update DummySort mode now contacts cd i (Object_ sort o) = do
        (f, o') <- update sort mode now contacts cd i o
        return (f, Object_ sort o')
    updateNoSceneChange DummySort mode now contacts cd (Object_ sort o) =
        Object_ sort <$> updateNoSceneChange sort mode now contacts cd o
    render = error "Don't use this function, use render_ instead (that's type safe)"

sort_ :: Object_ -> Sort_
sort_ (Object_ sort _) = Sort_ sort

-- | object rendering without providing the sort
render_ :: Object_ -> Ptr QPainter -> Offset Double -> Seconds -> IO ()
render_ (Object_ sort o) = render o sort

wrapObjectModifier :: Sort s o => (o -> o) -> Object_ -> Object_
wrapObjectModifier f (Object_ s o) =
    case (cast s, cast o) of
        (Just s_, Just o_) -> Object_ s_ (f o_)

-- * EditorObject

mkEditorObject :: Sort_ -> EditorPosition -> EditorObject Sort_
mkEditorObject sort pos =
    EditorObject sort pos oemState
  where
    oemState = fmap (\ methods -> oemInitialize methods pos) $ objectEditMode sort


renderChipmunk :: Ptr QPainter -> Offset Double -> Pixmap -> Chipmunk -> IO ()
renderChipmunk painter worldOffset p chipmunk = do
    (position, angle) <- getRenderPosition chipmunk
    renderPixmap painter worldOffset position (Just angle) p


-- * Object edit mode

unpickleOEM :: Sort_ -> String -> OEMState
unpickleOEM (objectEditMode -> Just methods) = oemUnpickle methods
