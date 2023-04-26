module Base.Fields where

import qualified Data.Map as Map (Map, fromList, lookup)

data Field
  -- Boolean flags
  = Revealed
  -- Enums
  | Owner | Zone
  -- Integers
  | SetID | CardNum
  deriving (Eq, Ord, Show, Enum, Bounded)
  -- TODO: custom ordering

type FieldMap = Map.Map Field Int

-- Various Boolean flags for a card
type Revealed = Bool

-- Various Enum values for a card
data Owner = System | P1 | P2
  deriving (Eq, Ord, Enum, Show)

data Zone = Deck | Hand
  deriving (Eq, Ord, Enum, Show)

-- Various Integer values for a card
type SetID = Int
type CardNum = Int
