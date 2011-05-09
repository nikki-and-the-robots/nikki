{-# language MultiParamTypeClasses, FunctionalDependencies, NamedFieldPuns,
     ViewPatterns, ExistentialQuantification, DeriveDataTypeable #-}

module Object.Types where

import Utils

import Data.Dynamic

import Graphics.Qt as Qt

import Physics.Chipmunk hiding (Position, collisionType)

import Base


-- | object rendering without providing the sort
renderObject_ :: Object_ -> Ptr QPainter -> Offset Double -> Seconds -> IO [RenderPixmap]
renderObject_ (Object_ sort o) = renderObject o sort

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
    (position, angle) <- getRenderPositionAndAngle chipmunk
    renderPixmap painter worldOffset position (Just angle) p


-- * Object edit mode

unpickleOEM :: Sort_ -> String -> OEMState
unpickleOEM (objectEditMode -> Just methods) = oemUnpickle methods
