
module Base.Renderable.Menu (
    menuAppState,
    treeToMenu,
  ) where


import Data.SelectTree (SelectTree(..))
import qualified Data.Indexable as I

import Control.Arrow

import Graphics.Qt

import Utils

import Base.Types hiding (selected)
import Base.Application
import Base.Prose
import Base.Font ()

import Base.Renderable.Common
import Base.Renderable.VBox
import Base.Renderable.WholeScreenPixmap
import Base.Renderable.Layered
import Base.Renderable.Centered
import Base.Renderable.CenterHorizontally


data Menu
    = Menu {
        title :: String,
        before :: [(String, AppState)],
        selected :: (String, AppState),
        after :: [(String, AppState)]
      }

mkMenu :: String -> [(String, Int -> AppState)] -> Int -> Menu
mkMenu title items =
    inner $ zipWith (\ (label, appStateFun) n -> (label, appStateFun n)) items [0..]
  where
    inner items n =
        if n < 0 then
            inner items 0
          else if n > length items - 1 then
            inner items (length items - 1)
          else
            let (before, selected : after) = splitAt n items
            in Menu title before selected after

selectNext :: Menu -> Menu
selectNext (Menu t b s (a : r)) = Menu t (b +: s) a r
selectNext m@(Menu _ _ _ []) = m

selectPrevious :: Menu -> Menu
selectPrevious m@(Menu _ [] _ _) = m
selectPrevious (Menu t b s a) = Menu t (init b) (last b) (s : a)

-- | Creates a menu with a title, if given.
-- If a parent is given, the menu can be aborted to go to the parent state.
-- The prechoice will determine the initially selected menu item.
menuAppState :: Application_ sort -> String -> Maybe AppState
    -> [(String, Int -> AppState)] -> Int -> AppState
menuAppState app title mParent children preSelection =
    inner $ mkMenu title children preSelection
  where
    inner :: Menu -> AppState
    inner items = appState (menuRenderable items) $ do
        e <- waitForPressButton app
        if isUp e then return $ inner $ selectPrevious items
         else if isDown e then return $ inner $ selectNext items
         else if isMenuConfirmation e then return $ snd $ selected items
         else if isBackButton e then case mParent of
                Just parent -> return parent
                Nothing -> return $ inner items
         else return $ inner items

    -- B button (keyboard or gamepad) or Escape
    isBackButton x = isBButton x || isKey Escape x

    -- Enter or A button
    isMenuConfirmation x =
        isAButton x ||
        isEnterOrReturn x


-- * automatic creation

-- | convert a SelectTree to a menu
treeToMenu :: Application_ sort -> AppState -> SelectTree String -> (String -> AppState)
    -> Int -> AppState
treeToMenu app parent (Leaf n) f _ = f n
treeToMenu app parent (Node label children i) f preSelection =
    menuAppState app label (Just parent) (map mkItem (I.toList children)) preSelection
  where
    mkItem :: SelectTree String -> (String, Int -> AppState)
    mkItem t = (getLabel t, \ ps -> treeToMenu app (this ps) t f 0)
    getLabel (Leaf n) = n
    getLabel (Node n _ _) = n

    this = treeToMenu app parent (Node label children i) f


-- * rendering

menuRenderable items =
    MenuBackground |:>
    (centered $ vBox $ fmap centerHorizontally $ toLines items)

toLines :: Menu -> [Prose]
toLines (Menu title before selected after) = map p (
    title : "" :
    map fst before ++
    ("-> " ++ fst selected ++ " <-") :
    map fst after)

-- | modify the items before the selected to implement simple scrolling
mkScrolling :: [(String, AppState)] -> [(String, AppState)]
mkScrolling before = drop (max 0 (length before - 4)) before

