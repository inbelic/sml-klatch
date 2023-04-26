module Tests.Filters
  ( prop_contrast
  , prop_composite
  , filterGroup
  ) where

{-# LANGUAGE OverloadedStrings #-}

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Internal.Filters
import Internal.Types (CardState)
import Tests.Gens

import qualified Data.Map as Map (union, intersection, keys, filter)
import qualified Data.Set as Set (intersection, toList)

-- We want to ensure that the contrast or switch of a filter gets all
-- other cards of a filter that are in the valid set. In this case the
-- valid set is all the cards that have the field in their field map
prop_contrast :: Property
prop_contrast =
  property $ do
    cardState <- forAll genCardState
    filter@(Filter field _ _) <- forAll genFilter
    let subset1 = trim filter cardState
        subset2 = trim (switch filter) cardState
        validSet = Map.filter (elem field . Map.keys) cardState
    -- Ensure the filtered sets are disjoint
    assert (null $ Map.keys (Map.intersection subset1 subset2))
    -- Ensure the filtered sets are the entire valid set when combined
    assert $ Map.keys (Map.union subset1 subset2) == Map.keys validSet

-- Ensure that the filters are being applied as though they were 'and'
-- expressions, as such if we have a filter and its switch in the same filter
-- set then we should never return any cards
prop_composite :: Property
prop_composite =
  property $ do
    filters <- forAll genCover
    cardState <- forAll genCardState
    assert . null . Map.keys . cut filters $ cardState

filterGroup :: Group
filterGroup =
  Group "Test.Filters"
    [ ("Checking that contrast filters are complimentry", prop_contrast)
    , ("Checking that filters are 'and' composable", prop_composite)
    ]
