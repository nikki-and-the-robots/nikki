{-# language ViewPatterns #-}

-- Module for trees with one selected item, that can be stepped through

module Data.SelectTree (
    SelectTree(..),
    mkNode,
    addChild,
    labelA,
    mapLabels,
    getChildren,
    getSelected,
    selectNext,
    selectPrevious,
    selectFirstElement,
    leafs,
    modifyLabelled,
    (-<),
    readDirectoryToSelectTree,
  ) where

import Utils

import Data.Maybe
import qualified Data.Indexable as I
import Data.Indexable hiding (length, toList, findIndices, fromList, catMaybes)
import qualified Data.Tree as T
import Data.Foldable (Foldable, foldMap)
import Data.Traversable (Traversable(..))
import Data.Accessor

import System.Directory
import System.FilePath


data SelectTree a
    -- Invariant: length [SelectTree a] > Index
    = Node String (Indexable (SelectTree a)) Index
    | Leaf String a
    | EmptyNode String
  deriving Show


instance Functor SelectTree where
    fmap f (Node label children index) = Node label (fmap (fmap f) children) index
    fmap f (Leaf l a) = Leaf l $ f a
    fmap f (EmptyNode l) = EmptyNode l

instance Show a => PP (SelectTree a) where
    pp = T.drawTree . toTree

instance Foldable SelectTree where
    foldMap f (Leaf _ a) = f a
    foldMap f (Node _ cs _) = foldMap (foldMap f) cs

instance Traversable SelectTree where
    traverse f (Node l ixs i) =
        Node l <$> traverse (traverse f) ixs <*> pure i
    traverse f (Leaf l a) =
        Leaf l <$> f a
    traverse f (EmptyNode l) = pure (EmptyNode l)

mkNode :: String -> [SelectTree a] -> SelectTree a
mkNode label [] = EmptyNode label
mkNode label (I.fromList -> ix) = Node label ix (head (keys ix))

-- | adds a child (at the end)
-- PRE: the second given tree is not a Leaf
addChild :: SelectTree a -> SelectTree a -> SelectTree a
addChild _ (Leaf _ _) = error "addChild"
addChild x (EmptyNode label) = mkNode label [x]
addChild x (Node label children selected) = Node label (children >: x) selected

getLabel :: SelectTree a -> String
getLabel (Node l _ _) = l
getLabel (Leaf l _) = l
getLabel (EmptyNode l) = l
setLabel :: String -> SelectTree a -> SelectTree a
setLabel l (Node _ a b) = Node l a b
setLabel l (EmptyNode _) = EmptyNode l
setLabel l (Leaf _ e) = Leaf l e

labelA :: Accessor (SelectTree a) String
labelA = accessor getLabel setLabel

mapLabels :: (String -> String) -> SelectTree a -> SelectTree a
mapLabels f (Node label a b) = Node (f label) (fmap (mapLabels f) a) b
mapLabels f (Leaf label x) = Leaf (f label) x
mapLabels f (EmptyNode label) = EmptyNode (f label)

getChildren :: SelectTree a -> [SelectTree a]
getChildren (Node _ x _) = I.toList x
getChildren _ = []

getSelected :: SelectTree a -> a
getSelected (Node sn cs i) = getSelected (cs !!! i)
getSelected (Leaf _ a) = a

-- | selects the previous item in the tree.
-- steps through all the items in a tree.
-- wraps around in the beginning.
selectPrevious :: SelectTree a -> SelectTree a
selectPrevious = selectOther (subtract 1) resetSelectedLast

-- | selects the next item in the tree.
-- steps through all the items in a tree.
-- wraps around in the end.
selectNext :: SelectTree a -> SelectTree a
selectNext = selectOther (+ 1) resetSelectedsFirst

selectOther :: (Index -> Index) -> (SelectTree a -> SelectTree a) -> SelectTree a -> SelectTree a
selectOther updateSelected reset tree =
    case selectNextNotWrapping tree of
        -- wraps around (the whole tree, not subtrees, of course)
        Nothing -> reset tree
        -- child is updated
        Just new -> new
  where
    selectNextNotWrapping :: SelectTree a -> Maybe (SelectTree a)
    selectNextNotWrapping (Node label children selected) =
        case selectNextNotWrapping (children !!! selected) of
            -- child cannot be updated -> parent must be updated
            Nothing ->  let newSelected = updateSelected selected
                        in if newSelected `isIndexOf` children then
                            Just $ Node label children newSelected
                          else
                            Nothing
            -- child could be updated: must be inserted in list
            Just child ->
                Just $ Node label (indexA selected ^: const child $ children) selected
    selectNextNotWrapping (Leaf _ a) = Nothing

-- resets the tree to select the first item in it and every child.
resetSelectedsFirst :: SelectTree a -> SelectTree a
resetSelectedsFirst (Leaf l a) = Leaf l a
resetSelectedsFirst (Node label children _) = Node label (fmap resetSelectedsFirst children) 0

resetSelectedLast :: SelectTree a -> SelectTree a
resetSelectedLast (Leaf l a) = Leaf l a
resetSelectedLast (Node label children _) =
    Node label (fmap resetSelectedLast children) (Index (I.length children - 1))


leafs :: SelectTree a -> [a]
leafs (Node _ ll _) = concatMap leafs (I.toList ll)
leafs (Leaf _ a) = [a]
leafs (EmptyNode l) = []


selectFirstElement :: (e -> Bool) -> SelectTree e -> Maybe (SelectTree e)
selectFirstElement f tree = inner (length $ leafs tree) tree
  where
    inner n _ | n <= 0 = Nothing
    inner n tree =
        if f (getSelected tree) then
            Just tree
          else
            inner (n - 1) (selectNext tree)


-- | modifies the children of a given tree with a given function, if it has a given label
-- PRE: given tree can not be a Leaf
modifyLabelled :: String -> (SelectTree a -> SelectTree a) -> SelectTree a -> SelectTree a
modifyLabelled testLabel f (Node label children selected) =
    Node label (fmap inner children) selected
  where
    inner n = if getLabel n == testLabel then f n else n
modifyLabelled _ _ x = x


toTree :: Show a => SelectTree a -> T.Tree String
toTree (Node snippet children _) = T.Node snippet (I.toList (fmap toTree children))
toTree (Leaf l a) = T.Node (l ++ ": " ++ show a) []
-- toTree x = es "toTree" x

(-<) :: SelectTree a -> Index -> SelectTree a
(Node _ children _) -< index = children !!! index

-- | Returns the files of a directory recursively as a SelectTree.
-- Returns just the files, omits directories
-- Returns just the files for which the given predicate is True.
readDirectoryToSelectTree :: (FilePath -> Bool) -> FilePath -> IO (SelectTree FilePath)
readDirectoryToSelectTree pred file = do
    result <- inner file
    return $ fromMaybe (error ("not a directory: " ++ file)) result
  where
    inner :: FilePath -> IO (Maybe (SelectTree FilePath))
    inner file = do
        isDir <- doesDirectoryExist file
        isFile <- doesFileExist file
        if isDir then do
            subFiles <- fmap (file </>) <$> getFiles file Nothing
            children <- catMaybes <$> fmapM inner subFiles
            return $ Just $ mkNode (file ++ "/") children
          else if isFile then
            return $ if pred file
                then Just $ Leaf file file
                else Nothing
          else
            error ("readDirectoryToSelectTree: file not found: " ++ file)
