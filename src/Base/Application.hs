
module Base.Application (
    Application_(..),
    AppState(..),
    ioAppState,
    executeStates,

    menu,
    mainMenu,
    treeToMenu,
    askStringRead,
    askString,
    drawTextBlock,
    waitAnyKey,
  ) where


import Safe

import Data.SelectTree hiding (selectPrevious, selectNext)
import qualified Data.Indexable as I

import Control.Monad
import Control.Concurrent

import Graphics.Qt

import Utils

import Base.GlobalCatcher
import Base.Events
import Base.Monad

import Base.Application.Pixmaps


-- from Top.Application

data Application_ sort
    = Application {
        application :: Ptr QApplication,
        window :: Ptr AppWidget,
        keyPoller :: KeyPoller,
        mainMenu_ :: Application_ sort -> AppState,
        applicationPixmaps :: ApplicationPixmaps,
        allSorts :: SelectTree sort
      }

mainMenu :: Application_ sort -> AppState
mainMenu app = mainMenu_ app app

data AppState
    = AppState (M AppState)
    | FinalState

-- | if you don't need the M monad, just IO
ioAppState :: IO AppState -> AppState
ioAppState = AppState . io


executeStates :: AppState -> M ()
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

menu :: Application_ sort -> Maybe String -> Maybe AppState -> [(String, AppState)] -> AppState
menu app mTitle mParent children =
    inner $ mkMenuItems children
  where
    inner items = ioAppState $ do
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

    render items ptr = do
        resetMatrix ptr
        clearScreen ptr
        setPenColor ptr white 1

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

treeToMenu :: Application_ sort -> AppState -> SelectTree String -> (String -> AppState)
    -> AppState
treeToMenu app parent (Leaf n) f = f n
treeToMenu app parent (Node label children i) f =
    menu app (Just label) (Just parent) (map mkItem (I.toList children))
  where
    mkItem t = (getLabel t, treeToMenu app this t f)
    getLabel (Leaf n) = n
    getLabel (Node n _ _) = n

    this = treeToMenu app parent (Node label children i) f

-- | Gets a string from the user.
-- returns the parent if Escape is pressed.
askString :: Application_ sort -> AppState -> String -> (String -> AppState) -> AppState
askString app parent question follower = ioAppState $ do
    answerRef <- newMVar ""
    setDrawingCallbackAppWidget (window app) (Just $ render question answerRef)
    loop answerRef
  where
    loop answerRef = do
        updateAppWidget $ window app
        event <- waitForAppEvent $ keyPoller app
        case event of
            Press StartButton ->
                return parent
            Press (KeyboardButton k _) | k == Return || k == Enter ->
                follower <$> readMVar answerRef
            Press (KeyboardButton k text) -> do
                modifyMVar_ answerRef (\ x -> return $ modifyTextField k text x)
                loop answerRef
            _ -> loop answerRef
    render :: String -> MVar String -> Ptr QPainter -> IO ()
    render question answerRef ptr = do
        resetMatrix ptr
        clearScreen ptr
        setPenColor ptr white 1

        let nextY = translate ptr (Position 0 20)
        nextY

        answer <- readMVar answerRef
        drawText ptr (Position 10 0) False (question ++ ": " ++ answer)

-- | Like askString, but reads (parses with Read) the given String. Asks again, if not parsable.
askStringRead :: Read a => Application_ sort -> AppState -> String -> (a -> AppState) -> AppState
askStringRead app parent question follower =
    askString app parent question wrapper
  where
    wrapper :: String -> AppState
    wrapper s = case readMay s of
        Nothing -> askStringRead app parent question follower -- try again
        Just r -> follower r

-- | waits for any key.
waitAnyKey :: Application_ s -> IO ()
waitAnyKey app = do
    e <- waitForAppEvent $ keyPoller app
    case e of
        Press _ -> return ()
        _ -> waitAnyKey app

drawTextBlock :: Ptr QPainter -> String -> IO ()
drawTextBlock ptr text = do
    mapM_ (\ (i, line) -> drawText ptr (Position 0 (i * 15)) False line) $ zip [0..] (lines text)
