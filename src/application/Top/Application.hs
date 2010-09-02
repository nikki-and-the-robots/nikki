
module Top.Application where


import Data.SelectTree hiding (selectPrevious, selectNext)
import qualified Data.Indexable as I

import Control.Monad

import Graphics.Qt

import Utils

import Base.GlobalCatcher
import Base.Events

import Object


data Application
    = Application {
        application :: Ptr QApplication,
        window :: Ptr AppWidget,
        keyPoller :: KeyPoller,
        sorts :: SelectTree Sort_
      }


data AppState
    = AppState (IO AppState)
    | FinalState

executeStates :: AppState -> IO ()
executeStates (AppState cmd) =
    cmd >>= executeStates
executeStates FinalState = return ()


data MenuItems
    = MenuItems {
        before :: [(String, AppState)],
        selected :: (String, AppState),
        after :: [(String, AppState)]
      }

mkMenuItems :: [(String, AppState)] -> MenuItems
mkMenuItems (a : r) = MenuItems [] a r

selectNext :: MenuItems -> MenuItems
selectNext (MenuItems b s (a : r)) = MenuItems (b +: s) a r
selectNext m@(MenuItems _ _ []) = m

selectPrevious :: MenuItems -> MenuItems
selectPrevious m@(MenuItems [] _ _) = m
selectPrevious (MenuItems b s a) = MenuItems (init b) (last b) (s : a)

menu :: Application -> Maybe String -> Maybe AppState -> [(String, AppState)] -> AppState
menu app mTitle mParent children =
    inner $ mkMenuItems children
  where
    inner items = AppState $ do
        setDrawingCallbackAppWidget (window app) (Just $ render items)
        event <- waitForAppEvent $ keyPoller app
        case event of
            Press UpButton -> return $ inner $ selectPrevious items
            Press DownButton -> return $ inner $ selectNext items
            Press AButton -> return $ snd $ selected items
            Press x | isBackButton x -> case mParent of
                Just parent -> return parent
                Nothing -> return $ inner items
            x -> return $ inner items

    isBackButton BButton = True
    isBackButton StartButton = True
    isBackButton _  = False

    render items ptr = globalCatcher $ do
        resetMatrix ptr
        clearScreen ptr
        setPenColor ptr 255 255 255 255 1

        let nextY = translate ptr (Position 0 yStep)

        nextY
        whenMaybe mTitle $ \ title ->
            drawText ptr (Position x 0) False title
        nextY
        nextY

        forM_ (before items) $ \ i -> do
            nextY
            drawText ptr (Position x 0) False (" " ++ fst i ++ " ")
        nextY
        drawText ptr (Position x 0) False ("[" ++ fst (selected items) ++ "]")
        forM_ (after items) $ \ i -> do
            nextY
            drawText ptr (Position x 0) False (" " ++ fst i ++ " ")

    yStep = 20
    x = 10

treeToMenu :: Application -> AppState -> SelectTree String -> (String -> AppState)
    -> AppState
treeToMenu app parent (Leaf n) f = f n
treeToMenu app parent (Node label children i) f =
    menu app (Just label) (Just parent) (map mkItem (I.toList children))
  where
    mkItem t = (getLabel t, treeToMenu app this t f)
    getLabel (Leaf n) = n
    getLabel (Node n _ _) = n

    this = treeToMenu app parent (Node label children i) f


