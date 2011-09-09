
module Base.Renderable.AskString (
    askString,
    askStringRead,
    mkAskStringWidget,
  ) where


import Safe

import Utils

import Graphics.Qt

import Base.Types
import Base.Polling
import Base.Prose

import Base.Configuration.Controls

import Base.Renderable.WholeScreenPixmap
import Base.Renderable.Layered
import Base.Renderable.Centered
import Base.Renderable.VBox
import Base.Renderable.Prose ()


-- | Gets a string from the user.
-- returns the parent if Escape is pressed.
askString :: Application -> AppState -> Prose -> (String -> AppState) -> AppState
askString app parent question follower =
    loop ""
  where
    loop :: String -> AppState
    loop answer = AppState (mkAskStringWidget question answer) $ do
        event <- waitForAppEvent app
        case event of
            Press e | isTextFieldBack e ->
                return parent
            Press e | isTextFieldConfirmation e ->
                return $ follower answer
            Press (KeyboardButton k text) ->
                return $ loop $ modifyTextField k text answer
            _ -> return $ loop answer

mkAskStringWidget :: Prose -> String -> RenderableInstance
mkAskStringWidget question answer =
    RenderableInstance (
        MenuBackground |:>
        (centered $ vBox 1 $ map (tuple False) text)
      )
  where
    text :: [Prose]
    text =
        (question +> pVerbatim ": ") :
        pVerbatim answer :
        []

-- | Like askString, but reads (parses with Read) the given String. Asks again, if not parsable.
askStringRead :: Read a => Application -> AppState -> Prose -> (a -> AppState) -> AppState
askStringRead app parent question follower =
    askString app parent question wrapper
  where
    wrapper :: String -> AppState
    wrapper s = case readMay s of
        Nothing -> askStringRead app parent question follower -- try again
        Just r -> follower r
