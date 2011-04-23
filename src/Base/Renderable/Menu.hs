
module Base.Renderable.Menu (
    menu,
    menuWithPreChoice,
    treeToMenu,
  ) where


import Data.SelectTree (SelectTree(..))
import qualified Data.Indexable as I

import Control.Arrow

import Graphics.Qt

import Utils

import Base.Types hiding (selected)
import Base.Prose
import Base.Font ()

import Base.Renderable.Common
import Base.Renderable.VBox
import Base.Renderable.WholeScreenPixmap
import Base.Renderable.Layered
import Base.Renderable.Centered


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

-- | like menuWithPreChoice but without the prechoice.
menu :: Application_ sort -> String -> Maybe AppState
    -> [(String, AppState)] -> AppState
menu app title parent children =
    menuWithPreChoice app title parent children' 0
  where
    children' = map (second const) children

-- | Creates a menu with a title, if given.
-- If a parent is given, the menu can be aborted to go to the parent state.
-- The prechoice will determine the initially selected menu item.
menuWithPreChoice :: Application_ sort -> String -> Maybe AppState
    -> [(String, Int -> AppState)] -> Int -> AppState
menuWithPreChoice app title mParent children preChoice =
    inner $ mkMenu title children preChoice
  where
    inner :: Menu -> AppState
    inner items = appState (mainMenuRenderable items) $ do
        e <- waitForPressAppEvent app
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
    -> AppState
treeToMenu app parent (Leaf n) f = f n
treeToMenu app parent (Node label children i) f =
    menu app label (Just parent) (map mkItem (I.toList children))
  where
    mkItem t = (getLabel t, treeToMenu app this t f)
    getLabel (Leaf n) = n
    getLabel (Node n _ _) = n

    this = treeToMenu app parent (Node label children i) f


-- * rendering

mainMenuRenderable items =
    MenuBackground |:>
    (centered $ vBox $ fmap centered $ toLines items)

toLines :: Menu -> [Prose]
toLines (Menu title before selected after) = map p (
    title : "" :
    map fst before ++
    ("-> " ++ fst selected ++ " <-") :
    map fst after)

-- | modify the items before the selected to implement simple scrolling
mkScrolling :: [(String, AppState)] -> [(String, AppState)]
mkScrolling before = drop (max 0 (length before - 4)) before

