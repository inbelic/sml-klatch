module Internal.Misc where

import qualified Data.Map as Map (Map, keys)

-- Equivalent to (|>)
apply :: a -> (a -> b) -> b
apply x f = f x

-- Simple function to allocate new IDs with no collision of
-- CardID, AbilityID or TargetIDs in their respective maps
getNextKey :: (k -> Int) -> Map.Map k a -> Int
getNextKey f = (+) 1 . maximum' . map f . Map.keys
  where
    maximum' :: [Int] -> Int
    maximum' [] = 0
    maximum' xs = maximum xs
