module Base.Fields where

import Internal.Cut

import qualified Data.Map as Map (Map, fromList, lookup, filter, keys)

data Field
  -- Boolean flags
  = Revealed
  -- Enums
  | Owner | Zone | Phase
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

data Phase = Morning | Seige | Retaliate | Nominate | Skirmish | Night
  deriving (Eq, Ord, Enum, Show)

-- Various Integer values for a card
type SetID = Int
type CardNum = Int

type NumEnums = Int
data FieldType
  = FlagType
  | EnumType NumEnums
  | IntType
  deriving Eq

-- Map the Fields to their corresponding type for the sake of documentation
-- and testing
fieldTypeMap :: Map.Map Field FieldType
fieldTypeMap = Map.fromList
  [ (Revealed, FlagType)  -- Boolean flags as either set (True) or not (False)
  ----------------------------------------
  , (Owner, EnumType 3)   -- Enums in the range [0, NumEnums - 1]
  , (Zone, EnumType 2)
  , (Phase, EnumType 6)
  ----------------------------------------
  , (SetID, IntType)      -- Integers are in the set [-128, 128]
  , (CardNum, IntType)
  ]

fieldToType :: Field -> FieldType
fieldToType field
  = case Map.lookup field fieldTypeMap of
      Nothing -> undefined  -- All Fields MUST have their type specified
      (Just x) -> x

-- Subsect the Fields into lists of their respective types
flagFields :: [Field]
flagFields = Map.keys . Map.filter f $ fieldTypeMap
  where
    f FlagType = True
    f _ = False

enumFields :: [Field]
enumFields = Map.keys . Map.filter f $ fieldTypeMap
  where
    f (EnumType _) = True
    f _ = False

intFields :: [Field]
intFields = Map.keys . Map.filter f $ fieldTypeMap
  where
    f IntType = True
    f _ = False

-- When we want to evaluate the fields of a card when viewing the cards we will
-- use a Window to show which Field we want to asses and the Filtration will
-- filter to a subset of cards that we want to evaluate (view).
--
-- Examples:
--
-- (Cost, Filtration [Constraint Zone Eq (fromEnum Hand)])
--  -> We want to evaluate the Cost field of all cards that are in the hand zone
--
-- (Toughness, Filtration [ Constraint Zone Eq (fromEnum Battlefield)
--                        , Constraint Cost LsEq 5
--                        ]) -> We want to evaluate the Toughness field of
-- all cards that are in the battlefield zone and have a toughness
-- less than or equal to 5
data Window = Window Field (Filtration Field)
  -- Field: Which Field of the card we want to evaluate
  -- Filters: Subset of cards that we want to evaluate


-- Compiled windows denote a set of Windows that are in order of their
-- dependencies, so if we consider the ith of n filters, then
-- i cannot have any dependencies of any filters from 0 to i - 1
--
-- We use a newtype so that we don't accidently use uncompiled filters
-- somewhere and enter an infinite loop of dependencies
newtype CompiledWindows = CWindows
  { getWindows :: [Window]
  }
