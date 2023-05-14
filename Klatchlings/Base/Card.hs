module Base.Card
  ( Card(..)
  , Cards
  , CardState
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
  -- Other external Functions
  , view
  , collectHeaders
  , lookupAbility
  , discardAlteration
  ) where

import Base.Fields
import Internal.Misc (apply, getNextKey)
import Internal.Types
import Internal.Filters

import qualified Data.Map as Map
  ( Map, empty, lookup, adjust
  , insert, insertWith
  , foldrWithKey, map, filter
  , intersection , intersectionWith
  )

-- Internal interface to a Card (should only be used by files in Internal)

-- Create a new empty card
create :: Card
create = Card Map.empty Map.empty Nothing

-- FIXME: Temprorary
--
-- Once all the initial field values and abilities have been set we can
-- 'mint' the card to store what its original state should be
mint :: Card -> Card
mint crd = crd{ original = Just crd{ original = Nothing} }

-- External interface to a Card

-- We can return the card to its original 'minted' original value
reload :: Change
reload = Change $ \crd ->
  case original crd of
    Nothing -> ([], crd)
    (Just oc) -> ([Reload], oc{ original = Just oc })

-- Shift the value of the Field by x
shift :: Field -> Int -> Change
shift fld x = Change $ \crd -> ([Shift fld x], shift' fld x crd)
  where
    shift' :: Field -> Int -> Card -> Card
    shift' fld x crd
      = let fields' = Map.adjust (wrap (+ x) <>) fld $ fields crd
         in crd{ fields = fields' }

-- Set the value of the Field to x
set :: Field -> Int -> Change
set fld x = Change $ \crd -> ([Set fld x], set' fld x crd)
  where
    set' :: Field -> Int -> Card -> Card
    set' fld x crd
      = let fields' = Map.insert fld (wrap $ const x) $ fields crd
         in crd{ fields = fields' }

-- Modify how a card evaluates the value of a the Field
-- if there is no eval already present then we will just use the given one,
-- otherwise, we compose the evaluation onto the old evaluation
modify :: Field -> Eval -> Change
modify fld ev = Change $ \crd -> ([Alter fld], modify' fld ev crd)
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
replace fld ev = Change $ \crd -> ([Alter fld], replace' fld ev crd)
  where
    replace' :: Field -> Eval -> Card -> Card
    replace' fld ev crd
      = let fields' = Map.insert fld ev $ fields crd
            in crd{ fields = fields' }

-- Give an ability to a card
-- See Misc.getNextKey for how the AbilityID is computed
equip :: Ability -> Change
equip ablty = Change $ \crd -> ([Equip], equip' ablty crd)
  where
    equip' :: Ability -> Card -> Card
    equip' ablty crd
      = let abltys = abilities crd
            aID = AbilityID . getNextKey abilityID $ abltys
            abltys' = Map.insert aID ablty abltys
         in crd { abilities = abltys' }



-- Given the Cards and the corresponding filters we can create our snapshot
-- of the current state of all the card field values
view :: Cards -> CompiledFilters -> CardState
view crds = foldr (view' crds) init . getFilters
  where
    init = Map.map (const Map.empty) crds

    view' :: Cards -> EvalFilter -> CardState -> CardState
    view' crds (fld, filters) cs
      = Map.foldrWithKey (eval fld) cs  -- fold over cards to evaluate field of
      . Map.intersection crds           -- retrieve corresponding cards
      . cut filters                     -- get CardIDs to evaluate
      $ cs

    eval :: Field -> CardID -> Card -> CardState -> CardState
    eval fld cID crd cs
      = case Map.lookup fld $ fields crd of
          Nothing -> cs
          (Just ev) -> let x = doEval ev cs 0
                        in Map.adjust (Map.insert fld x) cID cs

-- Here we go through and collect all the triggers that occur on the
-- current game state
collectHeaders :: GameState -> Cards -> [Header]
collectHeaders gs = Map.foldrWithKey (collectHeaders' gs) []
  where
    collectHeaders' :: GameState -> CardID -> Card -> [Header] -> [Header]
    collectHeaders' gs cID crd hdrs
      = Map.foldrWithKey (toHeader gs cID) hdrs -- map ability to a header
      . Map.filter (isTriggered gs cID)         -- filter triggered abilities
      . abilities $ crd

    isTriggered :: GameState -> CardID -> Ability -> Bool
    isTriggered gs cID ablty = checkTrigger (getTrigger ablty) cID gs

    toHeader :: GameState -> CardID -> AbilityID -> Ability -> [Header]
                  -> [Header]
    toHeader _ cID aID ablty
      | getTiming ablty == OnResolve
        = (:) (Unassigned cID aID)
      | otherwise                -- NOTE: fine as long as no other timings added
        = (:) (Assigned cID aID $ getTargets (getTargeting ablty) cID gs)

lookupAbility :: CardID -> AbilityID -> Cards -> Maybe Ability
lookupAbility cID aID =
  (=<<) (Map.lookup aID)
  . fmap abilities
  . Map.lookup cID

discardAlteration :: Change -> Card -> Card
discardAlteration = (snd .) . changes
