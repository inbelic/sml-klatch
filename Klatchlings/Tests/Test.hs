module Tests.Test where

{-# LANGUAGE OverloadedStrings #-}

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Tests.Gens

import Base.Fields

import qualified Data.Map as Map (fromList, keys)
import qualified Data.Set as Set (toList)

-- Dummy property to get started :)
prop_types :: Property
prop_types =
  property $ do
    fm <- forAll genFieldMap
    assert (0 <= fmCount fm)

fmCount :: FieldMap -> Int
fmCount = length . Map.keys

tests :: IO Bool
tests =
  checkParallel $ Group "Test.Example" [
      ("prop_types", prop_types)
    ]
