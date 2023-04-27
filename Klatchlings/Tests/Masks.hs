module Tests.Masks
  ( prop_cover
  , maskGroup
  ) where

{-# LANGUAGE OverloadedStrings #-}

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Internal.Load
import Internal.Types (CardState)
import Tests.Gens

import Data.List (singleton)
import qualified Data.Map as Map (union, intersection, keys, filter)
import qualified Data.Set as Set (intersection, toList)

-- Ensure that a Mask with a set of Filters will treat the sets as
-- 'OR' statements. Hence, a Mask of the form
-- Mask Field [[filter], [switch filter]] should always contain the
-- field being masked by this 'cover'
prop_cover :: Property
prop_cover =
  property $ do
    field <- forAll genField
    masks <- forAll $ fmap (map (Mask field . singleton)) genCover
    fieldMap <- forAll $ genFieldMapWith [field]
    assert $ Map.keys (filterMasks masks fieldMap) == [field]

maskGroup :: Group
maskGroup =
  Group "Test.Masks"
    [ ("Checking that contrasted masks cover", prop_cover)
    ]
