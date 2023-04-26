module Tests.Gens
  ( genFields
  , genFieldsWith
  , genFieldMap
  , genFieldMapWith
  ) where

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Base.Fields

import qualified Data.Map as Map (Map, fromList)
import qualified Data.Set as Set (Set, toList, fromList, union)

-- Equivalent to genFieldsWith []
genFields :: Gen (Set.Set Field)
genFields = Gen.set (Range.linear min max) Gen.enumBounded
  where
    min = fromEnum (minBound :: Field)
    max = fromEnum (maxBound :: Field)

-- Generates a subset of the list of all Fields in the enum but will
-- always have the list of Fields in the final set
genFieldsWith :: [Field] -> Gen (Set.Set Field)
genFieldsWith fields = Set.union (Set.fromList fields) <$> genFields

-- Equivalent to genFieldMapWith []
genFieldMap :: Gen FieldMap
genFieldMap = (fmap Map.fromList . mapM genFieldVal . Set.toList) =<< genFields

-- Generates a subset of the fields with the given Fields to use as keys and
-- then fills them according to their type
genFieldMapWith :: [Field] -> Gen FieldMap
genFieldMapWith fields
  = (fmap Map.fromList . mapM genFieldVal . Set.toList) =<< genFieldsWith fields


-- Helper gen functions
genFieldVal :: Field -> Gen (Field, Int)
genFieldVal field = (field, ) <$> genValue (fieldToType field)

-- Generate a value according to the specifictions in Base.Fields
genValue :: FieldType -> Gen Int
genValue FlagType = fromEnum <$> Gen.bool
genValue IntType = Gen.int $ Range.constant (-128) 128
genValue (EnumType num) = Gen.int $ Range.constant 0 num
