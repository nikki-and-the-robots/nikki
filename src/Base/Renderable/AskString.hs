
module Base.Renderable.AskString (
    askString,
    askStringParse,
    mkAskStringWidget,
  ) where


import Safe

import Data.Set (member)

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
import Base.Renderable.Message


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
            Press (KeyboardButton V _ mods) | ControlModifier `member` mods -> do
                -- paste shortcut
                clipped <- io $ textQClipboard $ window app
                let processClipped x = headDef "" $ dropWhile null $ lines x
                return $ loop $ (answer ++ processClipped clipped)
            Press (KeyboardButton k text _) ->
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

-- | Like askString, but reads the given String with the given function.
-- Asks again, if not parsable.
askStringParse :: Application -> AppState -> Prose
    -> (String -> Either [Prose] a) -> (a -> AppState) -> AppState
askStringParse app parent question parse follower =
    askString app parent question wrapper
  where
    wrapper :: String -> AppState
    wrapper s = case parse s of
        Left err ->
            message app err $ -- show error message
            askStringParse app parent question parse follower -- try again
        Right r -> follower r
