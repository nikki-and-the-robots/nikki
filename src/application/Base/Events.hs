
module Base.Events where


import Data.Map (Map, fromList, member, (!))
import Data.Word
import qualified Data.Set as Set
import Data.Set (Set, union, difference, insert, intersection, empty, delete)
import Data.IORef

import Control.Concurrent

import System.IO.Unsafe

import Graphics.Qt

import Utils


data AppEvent
    = Press AppButton
    | Release AppButton
  deriving (Eq, Ord, Show)

data AppButton
    = LeftButton
    | RightButton
    | UpButton
    | DownButton

    | AButton
    | BButton

    | StartButton -- calling of a Menu

    | KeyboardButton Key
  deriving (Eq, Ord, Show)

data ControlData = ControlData {
    pressed :: [AppEvent],
    held :: Set AppButton
  }
    deriving Show

allArrowButtons :: Set AppButton
allArrowButtons = Set.fromList [UpButton, RightButton, DownButton, LeftButton]




-- this is for joystick (and gamepad) stuff, will be used soon!
type JJ_Event = ()

{-# NOINLINE keyStateRef #-}
keyStateRef :: IORef ([AppEvent], Set AppButton)
keyStateRef = unsafePerformIO $ newIORef ([], empty)

-- | non-blocking polling of AppEvents
pollAppEvents :: KeyPoller -> IO ControlData
pollAppEvents poller = do
    (unpolledEvents, keyState) <- readIORef keyStateRef
    qEvents <- pollEvents poller
    let appEvents = concatMap (toAppEvent keyState) (map Left qEvents)
        keyState' = foldr (.) id (map updateKeyState appEvents) keyState
    writeIORef keyStateRef ([], keyState')
    return $ ControlData (unpolledEvents ++ appEvents) keyState'

-- | puts AppEvents back to be polled again
unpollAppEvents :: [AppEvent] -> IO ()
unpollAppEvents events = do
    (unpolledEvents, keyState) <- readIORef keyStateRef
    writeIORef keyStateRef (unpolledEvents ++ events, keyState)


-- | Blocking wait for the next event.
-- waits between polls
waitForAppEvent :: KeyPoller -> IO AppEvent
waitForAppEvent poller = do
    ControlData events _ <- pollAppEvents poller
    case events of
        (a : r) -> do
            unpollAppEvents r
            return a
        [] -> do
            threadDelay (round (0.01 * 10 ^ 6))
            waitForAppEvent poller


updateKeyState :: AppEvent -> Set AppButton -> Set AppButton
updateKeyState (Press   k) ll = insert k ll
updateKeyState (Release k) ll = delete k ll


toAppEvent :: Set AppButton -> Either QtEvent JJ_Event -> [AppEvent]
-- keyboard
toAppEvent _ (Left (KeyPress key)) | key `member` key2button =
    [Press (key2button ! key)]
toAppEvent _ (Left (KeyRelease key)) | key `member` key2button =
    [Release (key2button ! key)]
toAppEvent _ (Left (KeyPress key)) = [Press (KeyboardButton key)]
toAppEvent _ (Left (KeyRelease key)) = [Release (KeyboardButton key)]

-- joystick
-- toAppEvent _ (Right (JoyButtonDown 0 jbutton)) | jbutton `member` jbutton2button =
--     [Press   (jbutton2button ! jbutton)]
-- toAppEvent _ (Right (JoyButtonUp   0 jbutton)) | jbutton `member` jbutton2button =
--     [Release (jbutton2button ! jbutton)]
-- toAppEvent oldButtons (Right (JoyHatMotion  0 0 x)) =
--     calculateJoyHatEvents oldButtons x

-- else:


key2button :: Map Key AppButton 
key2button = fromList [
      (Ctrl, AButton)
    , (Shift, BButton)
    , (Escape, StartButton)

    , (LeftArrow, LeftButton)
    , (RightArrow, RightButton)
    , (UpArrow, UpButton)
    , (DownArrow, DownButton)
  ]


-- does not contain 
jbutton2button :: Map Word8 AppButton
jbutton2button = fromList [
    -- xbox controller
      (0, AButton)
    , (1, BButton)
    -- impact controller
    , (2, AButton)
    , (3, BButton)
  ]

-- returns the events from the gamepad hat,
-- given the buttons already reported as pressed
-- and the number returned by SDL for that hat.
-- That number is (up -> 1, right -> 2, down -> 4, left -> 8)
-- added up, if more than one is pressed
calculateJoyHatEvents :: Set AppButton -> Word8 -> [AppEvent]
calculateJoyHatEvents oldButtons n =
    Set.toList (Set.map Press presses `union` Set.map Release releases)
  where
    presses = cp `difference` oldArrowButtons
    releases = oldArrowButtons `difference` cp
    oldArrowButtons = allArrowButtons `intersection` oldButtons
    cp = currentlyPressed n

currentlyPressed :: Word8 -> Set AppButton
currentlyPressed = cp 8
  where
    cp :: Word8 -> Word8 -> Set AppButton
    cp 0 _ = Set.empty
    cp s n
      | n >= s = toButton s `insert` cp (s `div` 2) (n - s)
      | n < s = cp (s `div` 2) n
    toButton n = case n of
        1 -> UpButton
        2 -> RightButton
        4 -> DownButton
        8 -> LeftButton

