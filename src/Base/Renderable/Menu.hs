
module Base.Renderable.Menu (
    menuAppState,
    treeToMenu,
  ) where


import Data.SelectTree (SelectTree(..))
import qualified Data.Indexable as I

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
        title :: Maybe Prose, -- if Nothing it's the main menu
        before :: [(Prose, AppState)],
        selected :: (Prose, AppState),
        after :: [(Prose, AppState)]
      }

mkMenu :: Maybe Prose -> [(Prose, Int -> AppState)] -> Int -> Menu
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

-- | Creates a menu.
-- If a title is given, it will be displayed. If not, the main menu will be assumed.
-- If a parent is given, the menu can be aborted to go to the parent state.
-- The prechoice will determine the initially selected menu item.
menuAppState :: Application_ sort -> Maybe Prose -> Maybe AppState
    -> [(Prose, Int -> AppState)] -> Int -> AppState
menuAppState app title mParent children preSelection =
    inner $ mkMenu title children preSelection
  where
    inner :: Menu -> AppState
    inner items = appState (menuRenderable app items) $ do
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

menuRenderable app items =
    case title items of
        Just title ->
            -- normal menu
            MenuBackground |:>
            (centered $ vBox $ fmap centerHorizontally $ titleLines ++ toLines items)
          where
            titleLines = fmap renderable $
                title : pVerbatim " " : []
        Nothing ->
            -- main menu
            MenuBackground |:>
            (centered $ vBox $ fmap centerHorizontally
                (mainMenuPixmap : toLines items))
          where
            mainMenuPixmap = renderable $ menuTitlePixmap $ applicationPixmaps app

-- | return the items (entries) of the menu
toLines :: Menu -> [RenderableInstance]
toLines (Menu _ before selected after) = fmap renderable $
    map fst before ++
    (proseSelect $ fst selected) :
    map fst after
  where
    proseSelect :: Prose -> Prose
    proseSelect p = pVerbatim "⇨ " +> p +> pVerbatim " ⇦"

-- | modify the items before the selected to implement simple scrolling
mkScrolling :: [(Prose, AppState)] -> [(Prose, AppState)]
mkScrolling before = drop (max 0 (length before - 4)) before

