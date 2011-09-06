{-# language FunctionalDependencies, ViewPatterns, ExistentialQuantification #-}

module Object.Types where


import Data.Dynamic
import Data.SelectTree

import Graphics.Qt as Qt

import Physics.Chipmunk hiding (Position, collisionType)

import Utils

import Base


mkSortsSelectTree :: [Sort_] -> SelectTree Sort_
mkSortsSelectTree sorts =
    foldl (flip addSort) (EmptyNode "") sorts
    where
    addSort :: Sort_ -> SelectTree Sort_ -> SelectTree Sort_
    addSort sort t = addByPrefix prefix label sort t
        where
        sortIdParts = wordsBy ['/'] $ getSortId $ sortId sort
        prefix = init sortIdParts
        label = last sortIdParts

    -- | adds an element by a given prefix to a SelectTree. If branches with needed labels
    -- are missing, they are created.
    -- PRE: The tree is not a Leaf.
    addByPrefix :: [String] -> String -> a -> SelectTree a -> SelectTree a
    addByPrefix _ _ _ (Leaf _ _) = error "addByPrefix"
    addByPrefix (a : r) label x node =
        -- prefixes left: the tree needs to be descended further
        if any (\ subTree -> subTree ^. labelA == a) (getChildren node) then
            -- if the child already exists
            modifyLabelled a (addByPrefix r label x) node
            else
            -- the branch doesn't exist, it's created
            addChild (addByPrefix r label x (EmptyNode a)) node
    addByPrefix [] label x node =
        -- no prefixes left: here the element is added
        addChild (Leaf label x) node


-- | object rendering without providing the sort
renderObject_ :: Application -> Configuration
    -> Object_ -> Ptr QPainter -> Offset Double -> Seconds -> IO [RenderPixmap]
renderObject_ app config (Object_ sort o) = renderObject app config o sort

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
