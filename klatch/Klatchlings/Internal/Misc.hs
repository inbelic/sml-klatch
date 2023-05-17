module Internal.Misc where

import qualified Data.Map as Map (Map, keys)
import Data.List (sort)

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

-- Will reorder the elements of a list if the order is unique and
-- the length is the same
reorder :: [a] -> [Int] -> Maybe [a]
reorder elems idxs
  | uniqueSpan idxs && (length idxs == length elems)
    = Just . foldr (reorder' elems . flip (-) 1) [] $ idxs
  | otherwise = Nothing
  where
    reorder' :: [a] -> Int -> [a] -> [a]
    reorder' elems idx = (:) (elems !! idx)

uniqueSpan :: [Int] -> Bool
uniqueSpan idxs = (==) [1..l] . sort $ idxs
  where l = length idxs
