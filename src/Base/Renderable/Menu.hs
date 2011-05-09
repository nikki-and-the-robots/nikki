{-# language ScopedTypeVariables #-}

module Base.Renderable.Menu (
    menuAppState,
    treeToMenu,
  ) where


import Data.SelectTree (SelectTree(..))
import qualified Data.Indexable as I

import Control.Concurrent.MVar

import Graphics.Qt

import Utils

import Base.Types hiding (selected)
import Base.Pixmap
import Base.Application
import Base.Prose
import Base.Font
import Base.Monad

import Base.Configuration
import Base.Configuration.Controls

import Base.Renderable.Common
import Base.Renderable.VBox
import Base.Renderable.WholeScreenPixmap
import Base.Renderable.Layered
import Base.Renderable.Centered
import Base.Renderable.CenterHorizontally
import Base.Renderable.Header
import Base.Renderable.Spacer
import Base.Renderable.StickToBottom


data Menu
    = Menu {
        title :: Maybe Prose, -- if Nothing it's the main menu
        before :: [(Prose, AppState)],
        selected :: (Prose, AppState),
        after :: [(Prose, AppState)],
        scrolling :: MVar Int
      }

instance Show Menu where
    show menu = case title menu of
        Nothing -> "<Menu>"
        Just x -> "<Menu: " ++ unP x ++ ">"

mkMenu :: Maybe Prose -> [(Prose, Int -> AppState)] -> Int -> IO Menu
mkMenu title items =
    inner $ zipWith (\ (label, appStateFun) n -> (capitalizeProse label, appStateFun n)) items [0..]
  where
    inner items n =
        if n < 0 then
            inner items 0
          else if n > length items - 1 then
            inner items (length items - 1)
          else do
            let (before, selected : after) = splitAt n items
            scrollingRef <- newMVar 0
            return $ Menu title before selected after scrollingRef

selectNext :: Menu -> Menu
selectNext (Menu t b s (a : r) sc) = Menu t (b +: s) a r sc
selectNext m@(Menu _ _ _ [] _) = m

selectPrevious :: Menu -> Menu
selectPrevious m@(Menu _ [] _ _ _) = m
selectPrevious (Menu t b s a sc) = Menu t (init b) (last b) (s : a) sc

-- | Creates a menu.
-- If a title is given, it will be displayed. If not, the main menu will be assumed.
-- If a parent is given, the menu can be aborted to go to the parent state.
-- The prechoice will determine the initially selected menu item.
menuAppState :: Application_ sort -> Maybe Prose -> Maybe AppState
    -> [(Prose, Int -> AppState)] -> Int -> AppState
menuAppState app title mParent children preSelection = NoGUIAppState $ io $
    inner <$> mkMenu title children preSelection
  where
    inner :: Menu -> AppState
    inner menu = appState menu $ do
        e <- waitForPressButton app
        controls_ <- gets controls
        if isMenuUp controls_ e then return $ inner $ selectPrevious menu
         else if isMenuDown controls_ e then return $ inner $ selectNext menu
         else if isMenuConfirmation controls_ e then return $ snd $ selected menu
         else if isMenuBack controls_ e then case mParent of
                Just parent -> return parent
                Nothing -> return $ inner menu
         else return $ inner menu

-- * automatic creation

-- | Converts a SelectTree to a menu.
-- Uses pVerbatim (and unP) to convert to (and from) Prose.
-- (Doesn't get translated therefore.)
treeToMenu :: Application_ sort -> AppState -> SelectTree String -> (String -> AppState)
    -> Int -> AppState
treeToMenu app parent (Leaf n) f _ = f n
treeToMenu app parent (Node label children i) f preSelection =
    menuAppState app (Just $ pVerbatim label) (Just parent)
        (map mkItem (I.toList children)) preSelection
  where
    mkItem :: SelectTree String -> (Prose, Int -> AppState)
    mkItem t = (pVerbatim $ getLabel t, \ ps -> treeToMenu app (this ps) t f 0)
    getLabel (Leaf n) = n
    getLabel (Node n _ _) = n

    this = treeToMenu app parent (Node label children i) f


-- * rendering

instance Renderable Menu where
    label = const "Menu"
    render ptr app config parentSize menu = do
        scrolling <- updateScrollingIO app parentSize menu
        let scroll = drop scrolling
        case title menu of
            Just title -> render ptr app config parentSize
                -- normal menu
                (MenuBackground |:>
                (addKeysHint (menuKeysHint True) $
                 centered $ vBox 4 $ addFrame $ fmap centerHorizontally lines))
              where
                lines = titleLine : lineSpacer : scroll (toLines menu)
                titleLine = header app title
            Nothing -> render ptr app config parentSize
                -- main menu
                (MenuBackground |:>
                (addKeysHint (menuKeysHint False) $
                 centered $ vBox 4 $ addFrame $ fmap centerHorizontally lines))
              where
                lines = mainMenuPixmap : lineSpacer : scroll (toLines menu)
                mainMenuPixmap = renderable $ menuTitlePixmap $ applicationPixmaps app

-- | return the items (entries) of the menu
toLines :: Menu -> [RenderableInstance]
toLines (Menu _ before selected after _) = fmap renderable $
    map fst before ++
    (colorizeProse white $ proseSelect $ fst selected) :
    map fst after
  where
    proseSelect :: Prose -> Prose
    proseSelect p = pVerbatim "⇨ " +> p +> pVerbatim " ⇦"

-- | adds a spacer before and after the menu
addFrame :: [RenderableInstance] -> [RenderableInstance]
addFrame ll = lineSpacer : ll +: lineSpacer 

-- | Returns the scrolling.
updateScrollingIO :: Application_ s -> Size Double -> Menu -> IO Int
updateScrollingIO app parentSize menu = do
    oldScrolling <- takeMVar $ scrolling menu
    let newScrolling = updateScrolling app parentSize menu oldScrolling
    putMVar (scrolling menu) newScrolling
    return newScrolling

updateScrolling :: Application_ s -> Size Double -> Menu -> Int -> Int
updateScrolling app parentSize menu oldScrolling =
    if itemsSpaceF >= 1 + 2 * itemPadding then
        min (allItems - itemsSpaceF) $
        max 0 $
        min (selectedIndex - itemPadding) $
        max oldScrolling $
        (selectedIndex - (itemsSpaceF - itemPadding - 1))
    else if itemsSpaceF > 0 then
        (selectedIndex - floor (fromIntegral (itemsSpaceF - 1) / 2 :: Double))
    else
        selectedIndex
  where
    -- space for the menu items in fontHeights
    itemsSpaceF :: Int = floor ((height parentSize - menuHeaderHeight) / fontHeight)
    -- height of the headers of the menu
    menuHeaderHeight = 2 * fontHeight + titleHeight
    titleHeight = case title menu of
        Nothing -> height $ pixmapSize $ menuTitlePixmap $ applicationPixmaps app
        Just _ -> headerHeight
    -- how many items should be visible ideally after or before the selected item
    itemPadding :: Int = 2
    -- index of the currently selected menu item
    selectedIndex = length $ before menu
    -- number of all menu items
    allItems :: Int = length (before menu) + 1 + length (after menu)
