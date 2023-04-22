module Base.Fields where

import qualified Data.Map as Map (Map)

data Field = Field
  deriving (Eq, Ord)
type FieldMap = Map.Map Field Int
