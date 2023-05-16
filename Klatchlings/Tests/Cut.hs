module Tests.Cut where

{-# LANGUAGE OverloadedStrings #-}

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Data.Map as Map (Map, empty, keys)
import           Data.List (sort)

import Internal.Cut
import Tests.Gens

prop_flippable :: Property
prop_flippable =
  property $ do
    cmp <- forAll (Gen.enumBounded :: Gen Comp)
    contrast (contrast cmp) === cmp

prop_contrasting :: Property
prop_contrasting =
  property $ do
    cmp <- forAll Gen.enumBounded
    val1 <- forAll . Gen.int . Range.constant (-128) $ 128
    val2 <- forAll . Gen.int . Range.constant (-128) $ 128
    let filt1 = mkFilt cmp
        filt2 = mkFilt . contrast $ cmp
    assert $ filt1 val1 val2 /= filt2 val1 val2

prop_complimentry :: Property
prop_complimentry =
  property $ do
    state <- forAll genCardState
    constraints <- forAll . Gen.list rng $ genConstraint
    let selection = Selection constraints
        filtration = Filtration . map contrast $ constraints
        selectKeys = Map.keys . select selection $ state
        filterKeys = Map.keys . filtrate filtration $ state
        allKeys = sort . Map.keys $ state
    -- Ensure we get the accumlated set of keys
    allKeys === sort (selectKeys ++ filterKeys)
    -- Ensure we don't return the same key in both sets
    [] === intersection selectKeys filterKeys
  where
    intersection xs = filter (`elem` xs)
    rng = Range.linear 0 128 

cutGroup :: Group
cutGroup = Group "Tests.Cut"
  [ ("Contrast . Contrast = id", prop_flippable)
  , ("Contrasting filters /=", prop_contrasting)
  , ("Complimentry filtration and selections", prop_complimentry)
  ]
