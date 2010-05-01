
module Graphics.Qt.Events (
    QtEvent(..),
    Key(..),
    translateQtKey,
  ) where

import Data.Map

import Graphics.Qt.Types


data QtEvent
    = KeyPress Key
    | KeyRelease Key
  deriving (Eq, Show)



data Key
    -- characters
    = A
    | B
    | C
    | D
    | E
    | F
    | G
    | H
    | I
    | J
    | K
    | L
    | M
    | N
    | O
    | P
    | Q
    | R
    | S
    | T
    | U
    | V
    | W
    | X
    | Y
    | Z

    -- international characters
    | AUmlaut
    | OUmlaut
    | UUmlaut
    | SZLigatur

    -- signs
    | Space
    | Comma
    | Dot
    | SemiColon
    | Slash
    | Plus
    | Minus
    | Equals
    | Tick
    | BackSlash
    | SquareBracketOpen
    | SquareBracketClose

    -- special keys
    | Enter
    | Escape
    | Tab
    | Delete
    | BackSpace

    -- arrow keys
    | UpArrow
    | DownArrow
    | LeftArrow
    | RightArrow

    -- modifiers
    | Mod
    | Shift
    | Ctrl
    | Alt
    | AltGr

    -- numbers
    | K1
    | K2
    | K3
    | K4
    | K5
    | K6
    | K7
    | K8
    | K9
    | K0

    | UnknownKey QtInt

  deriving (Eq, Ord, Show)

translateQtKey :: QtInt -> Key
translateQtKey keyInt | keyInt `member` keyMap =
    keyMap ! keyInt
translateQtKey x = UnknownKey x -- error ("translateQtKey: " ++ show x)

keyMap :: Map QtInt Key
keyMap = fromList keyMapping

keyMapping :: [(QtInt, Key)]
keyMapping = [
    -- characters
    (65, A),
    (66, B),
    (67, C),
    (68, D),
    (69, E),
    (70, F),
    (71, G),
    (72, H),
    (73, I),
    (74, J),
    (75, K),
    (76, L),
    (77, M),
    (78, N),
    (79, O),
    (80, P),
    (81, Q),
    (82, R),
    (83, S),
    (84, T),
    (85, U),
    (86, V),
    (87, W),
    (88, X),
    (89, Y),
    (90, Z),

    -- international characters
    (196, AUmlaut),
    (214, OUmlaut),
    (220, UUmlaut),
    (223, SZLigatur),

    -- signs
    (32, Space),
    (44, Comma),
    (46, Dot),
    (59, SemiColon),
    (47, Slash),
    (45, Minus),
    (61, Equals),
    (91, SquareBracketOpen),
    (93, SquareBracketClose),
    (92, BackSlash),
    (180, Tick),
    (43, Plus),

    -- special keys
    (16777220, Enter),
    (16777216, Escape),
    (16777217, Tab),
    (16777223, Delete),
    (16777219, BackSpace),

    -- arrow keys
    (16777235, UpArrow),
    (16777237, DownArrow),
    (16777234, LeftArrow),
    (16777236, RightArrow),

    -- modifiers
    (16777250, Mod),
    (16777248, Shift),
    (16777249, Ctrl),
    (16777251, Alt),
    (16781571, AltGr),

    -- numbers
    (49, K1),
    (50, K2),
    (51, K3),
    (52, K4),
    (53, K5),
    (54, K6),
    (55, K7),
    (56, K8),
    (57, K9),
    (48, K0)
  ]
