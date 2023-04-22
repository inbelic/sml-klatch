module Internal.Filters
  ( Filter(..)
  , Filters
  , EvalFilter
  , CompiledFilters(..)
  , trim
  , cut
  , Comp(..)
  , mkFilt
  , contrast
  ) where

import Base.Fields
import Internal.Types
import qualified Data.Map as Map (Map, filter, lookup)

-- We can think of a filter as a cut through the feasible planes
-- of all cards. Similar to the cutting-plane method
--
-- So a single filter will say we want all cards such that their
-- Field value, x, evaluates to Comp x Int = True
--
-- Examples:
--  Filter Power 4 Gr -> Would filter the set of cards to only include
--  cards that have their Power field set and greater than 4
--
--  Filter CardID 1 Eq - Since the CardID should be unique then it should
--  return the singleton list of the Card with CardID = 1
data Filter = Filter Field Int Comp
type Filters = [Filter]

-- When we want to evaluate the fields of a card we will use an EvalFilter
-- to show which Field we want to asses and the Filters will filter to
-- a subset of cards that we want to evaluate.
--
-- Examples:
--
-- (Cost, [Filter Zone (fromEnum Hand) Eq]) -> We want to evaluate
-- the Cost field of all cards that are in the hand zone
--
-- (Toughness, [ Filter Zone (fromEnum Battlefield) Eq
--             , Filter Cost 5 LsEq
--             ]) -> We want to evaluate the Toughness field of
-- all cards that are in the battlefield zone and have a toughness
-- less than or equal to 5
type EvalFilter = (Field, Filters)
  -- Field: Which Field of the card we want to evaluate
  -- Filters: Subset of cards that we want to evaluate

-- Compiled filters denote filters that are in order of their
-- dependencies, so if we consider the ith of n filters, then
-- i cannot have any dependencies of any filters from 0 to i - 1
--
-- We use a newtype so that we don't accidently use uncompiled filters
-- somewhere and enter an infinite loop of dependencies
newtype CompiledFilters = CFilters
  { getFilters :: [EvalFilter]
  }

-- Comparison related things
-- Enumeration of possible comparisons
data Comp = Eq | Neq | Ls | Gr | LsEq | GrEq | Tr | Fl
  deriving (Eq, Show)

-- Function to convert the enumeration to the function
mkFilt :: (Ord a) => Comp -> a -> a -> Bool
mkFilt Eq    = (==)
mkFilt Neq    = (/=)
mkFilt Ls    = (<)
mkFilt Gr    = (>)
mkFilt LsEq  = (<=)
mkFilt GrEq  = (>=)
-- Catch alls
mkFilt Tr    = \_ _ -> True
mkFilt Fl    = \_ _ -> False


-- When we compile filters we need to ensure that we evaluate all the
-- fields of all cards. So we can use the enumerations to create matching
-- filters that will correspond to the full feasible region being covered
contrast :: Comp -> Comp
contrast Eq = Neq
contrast Neq = Eq
contrast Ls = GrEq
contrast GrEq = Ls
contrast Gr = LsEq
contrast LsEq = Gr
contrast Tr = Fl
contrast Fl = Tr

-- Take a filter and return the subset of cards that satisfy the filter
-- condition
trim :: Filter -> CardState -> CardState
trim filt = Map.filter (trim' filt)
  where
    trim' :: Filter -> FieldMap -> Bool
    trim (Filter _ _ Tr) _ = True
    trim (Filter _ _ Fl) _ = False
    trim' (Filter field thres comp) fm
      = case Map.lookup field fm of
          Nothing -> False
          (Just x) -> mkFilt comp x thres

-- Do a series of trims
cut :: Filters -> CardState -> CardState
cut = flip (foldr trim)
