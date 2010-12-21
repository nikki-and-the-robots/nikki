
module Graphics.Qt.Events (
    QtEvent(..),
    Key(..),
    translateQtKey,
    modifyTextField,
  ) where


import Data.Map

import Graphics.Qt.Types


data QtEvent
    = KeyPress Key String
    | KeyRelease Key String
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
    | Return
    | Enter -- num pad
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
    | Meta
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


-- | modifies the contents of a text field
modifyTextField :: Key -> String -> String -> String
modifyTextField BackSpace _ [] = []
modifyTextField BackSpace _ l = init l
modifyTextField _ t l = l ++ t



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
    (0x01000004, Return),
    (0x01000005, Enter),
    (0x01000000, Escape),
    (0x01000001, Tab),
    (0x01000007, Delete),
    (0x01000003, BackSpace),

    -- arrow keys
    (0x1000013, UpArrow),
    (0x1000015, DownArrow),
    (0x1000012, LeftArrow),
    (0x1000014, RightArrow),

    -- modifiers
    (0x1000022, Meta),
    (0x1000020, Shift),
    (0x1000021, Ctrl),
    (0x1000023, Alt),
    (0x1001103, AltGr),

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
