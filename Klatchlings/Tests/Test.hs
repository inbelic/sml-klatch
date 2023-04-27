module Tests.Test where

{-# LANGUAGE OverloadedStrings #-}

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Tests.Gens
import Tests.Filters
import Tests.Masks

import Base.Fields

import qualified Data.Map as Map (fromList, keys)
import qualified Data.Set as Set (toList)

tests :: IO Bool
tests = do
  checkParallel filterGroup
  checkParallel maskGroup
