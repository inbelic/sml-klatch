module Tests.Gens
  ( genField
  , genFields
  , genFieldsWith
  , genFieldMap
  , genFieldMapWith
  , genCardState
  , genCardStateWith
  , genFilter
  , genFilters
  , genCover
  ) where

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Base.Card
import Base.Fields
import Internal.Filters
import Internal.Types (CardID(..), CardState, Alteration(..), Change(..))

import qualified Data.Map as Map (Map, fromList)
import qualified Data.Set as Set (Set, toList, fromList, union, map)

genField :: Gen Field
genField = Gen.enumBounded

-- Equivalent to genFieldsWith []
genFields :: Gen (Set.Set Field)
genFields = Gen.set (Range.linear min max) genField
  where
    min = fromEnum (minBound :: Field)
    max = fromEnum (maxBound :: Field) + 1

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

genCardState :: Gen CardState
genCardState = genCardStateWith []

genCardStateWith :: [Field] -> Gen CardState
genCardStateWith fields =
  fmap Map.fromList                                 -- Convert to Map
  -- Gen a FieldMap for each CardID
  . (=<<) (mapM (\cID -> (cID,) <$> genFieldMapWith fields))
  . fmap (Set.toList . Set.map CardID)    -- Gen unique list of CardID's
  . Gen.set (Range.linear 10 20)
  $ genInt


-- Generate a random Filter
genFilter :: Gen Filter
genFilter =
  (=<<) (flip (<$>) Gen.enumBounded   -- Gen a comparison operator
        . uncurry Filter)             -- Wrap the values into a Filter
  . (=<<) genFieldVal                 -- Gen a valid value
  $ Gen.enumBounded                   -- Gen Field to filter with

genFilters :: Gen Filters
genFilters = Gen.list (Range.linear 0 10) genFilter

genCover :: Gen Filters
genCover = fmap addSwitch genFilter
  where
    addSwitch :: Filter -> Filters
    addSwitch filter = [filter, switch filter]


-- Helper gen functions
genFieldVal :: Field -> Gen (Field, Int)
genFieldVal field = (field, ) <$> genValue (fieldToType field)

-- Generate a value according to the specifictions in Base.Fields
genValue :: FieldType -> Gen Int
genValue FlagType = fromEnum <$> Gen.bool
genValue IntType = genInt
genValue (EnumType num) = Gen.int $ Range.constant 0 num

genInt :: Gen Int
genInt = Gen.int $ Range.constant (-128) 128

-- Generate various changes to a card
genChange :: Gen Change
genChange = undefined

genShift :: Gen Change
genShift =
  fmap (uncurry shift)
  . (=<<) (wrap genField)
  $ genInt

genSet :: Gen Change
genSet =
  fmap (uncurry set)
  . (=<<) (wrap genField)
  $ genInt

genModify :: Gen Change
genModify =
  fmap (uncurry modify)
  . (=<<) (wrap genField)
  $ genEval

genReplace :: Gen Change
genReplace =
  fmap (uncurry replace)
  . (=<<) (wrap genField)
  $ genEval

genEval :: Gen Eval
genEval = undefined

wrap :: Gen b -> a -> Gen (b, a)
wrap gen x = (, x) <$> gen
