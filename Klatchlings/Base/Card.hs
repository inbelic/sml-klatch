module Base.Card
  ( Card(..)
  , Cards
  , Eval(..)
  , Alteration(..)
  , Change
  -- Card Interface
  , create
  , mint
  , reload
  , shift
  , set
  , replace
  , modify
  , equip
  ) where

import Base.Fields
import Internal.Misc (apply, getNextKey)
import Internal.Types
import Internal.Filters

import qualified Data.Map as Map
  ( Map, empty , map, lookup
  , insert, insertWith, adjust
  , intersection , intersectionWith
  )

-- External interface to a Card

-- Create a new empty card
create :: Card
create = Card Map.empty Map.empty Nothing

-- FIXME: Temprorary
--
-- Once all the initial field values and abilities have been set we can
-- 'mint' the card to store what its original state should be
mint :: Card -> Card
mint crd = crd{ original = Just crd{ original = Nothing} }

-- We can return the card to its original 'minted' original value
reload :: Card -> Card
reload crd = case original crd of
               Nothing -> crd
               (Just oc) -> oc{ original = Just oc }

-- Shift the value of the Field by x
shift :: Field -> Int -> Change
shift fld x crd = (Shift fld x, shift' fld x crd)
  where
    shift' :: Field -> Int -> Card -> Card
    shift' fld x crd
      = let fields' = Map.adjust (wrap (+ x) <>) fld $ fields crd
         in crd{ fields = fields' }

-- Set the value of the Field to x
set :: Field -> Int -> Change
set fld x crd = (Set fld x, set' fld x crd)
  where
    set' :: Field -> Int -> Card -> Card
    set' fld x crd
      = let fields' = Map.insert fld (wrap $ const x) $ fields crd
         in crd{ fields = fields' }

-- Modify how a card evaluates the value of a the Field
-- if there is no eval already present then we will just use the given one
modify :: Field -> Eval -> Change
modify fld ev crd = (Alter fld, modify' fld ev crd)
  where
    modify' :: Field -> Eval -> Card -> Card
    modify' fld ev crd
      = let fields' = Map.insertWith (<>) fld ev $ fields crd
            in crd{ fields = fields' }

-- Replace how a card evaluates the value of a the Field
-- NOTE: Provided to allow for more efficient implemenation so we don't need
-- to store references to the underlying eval chain that will no longer need
-- to be computed
replace :: Field -> Eval -> Change
replace fld ev crd = (Alter fld, replace' fld ev crd)
  where
    replace' :: Field -> Eval -> Card -> Card
    replace' fld ev crd
      = let fields' = Map.insert fld ev $ fields crd
            in crd{ fields = fields' }

-- Give an ability to a CardID
-- See Misc.getNextKey for how the AbilityID is computed
equip :: Ability -> Change
equip ablty = (Equip, ) . equip' ablty
  where
    equip' :: Ability -> Card -> Card
    equip' ablty crd
      = let abltys = abilities crd
            aID = AbilityID . getNextKey abilityID $ abltys
            abltys' = Map.insert aID ablty abltys
         in crd { abilities = abltys' }
