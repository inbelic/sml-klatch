module Base.CardState
  ( check
  ) where

import Base.Fields

import Internal.Types

import qualified Data.Map as Map (lookup)

-- Checks the integer value stored in the cards field
-- will return the provided default in that the card or field
-- value is not given
check :: CardID -> Field -> Int -> CardState -> Int
check cID fld def cs
  = case (=<<) (Map.lookup fld) $ Map.lookup cID cs of
      Nothing -> def
      (Just x) -> x
