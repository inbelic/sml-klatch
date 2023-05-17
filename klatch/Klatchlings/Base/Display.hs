module Base.Display
  ( displayStateCallback
  ) where

import Base.Card (CardState)
import Base.Fields
import Base.GameState (GameState(..))
import Internal.Load (LoadInfo(..), CompiledMasks(..), Masks, filterMasks)
import Internal.Types (CardID(..))

import qualified Data.Map as Map (Map, foldrWithKey, lookup)

-- Here we can define our custom way of displaying the current game
-- state that will be sent to the players.
--
-- The outputted string will be parsed by the external server side code
-- and will send the string encompassed by the first {} to one player
-- and all contents in the second {} to the other player. It will scan
-- for the closing bracket and count the number of opening and closing
-- brackets until there are equal. Hence, any implementation MUST ensure
-- that all { brackets have a corresponding closing }.

displayStateCallback :: LoadInfo -> GameState -> String
displayStateCallback loadInfo (GameState _ _ cardState)
  = displayPlayer P1 masks cardState ++ displayPlayer P2 masks cardState
    where
      masks = compiledMasks loadInfo

      displayPlayer :: Owner -> CompiledMasks -> CardState -> String
      displayPlayer p masks cardState
        = "{" ++ Map.foldrWithKey (displayState p masks) "}" cardState


-- We then have our helper functions
displayState :: Owner -> CompiledMasks -> CardID -> FieldMap -> String -> String
displayState P1 (CompiledMasks (sys, owned, unowned)) cID fm acc
  = case toEnum <$> Map.lookup Owner fm of
      (Just System) -> displaySystem sys cID fm ++ acc
      (Just P1) -> displayOwned owned cID fm ++ acc
      (Just P2) -> displayOwned unowned cID fm ++ acc
      Nothing -> acc
displayState P2 (CompiledMasks (sys, owned, unowned)) cID fm acc
  = case toEnum <$> Map.lookup Owner fm of
      (Just System) -> displaySystem sys cID fm ++ acc
      (Just P1) -> displayOwned unowned cID fm ++ acc
      (Just P2) -> displayOwned owned cID fm ++ acc
      Nothing -> acc
displayState System _ _ _ acc = acc

-- Format our fieldmap as follows:
-- [Maybe CardID,Field1:Value1,Field2:Value2,...,FieldN:ValueN]
-- where the Fields are converted to their integer enum representation
-- and are ordered as specified in Fields
formatFieldMap :: Maybe CardID -> FieldMap -> String
formatFieldMap cID
  = (++) ("[" ++ cIDStr) . Map.foldrWithKey displayFieldMap' "]"
  where
    cIDStr = case cID of
               Nothing -> ""
               (Just (CardID x)) -> show x

    displayFieldMap' :: Field -> Int -> String -> String
    displayFieldMap' fld x acc
      = "," ++ show (fromEnum fld) ++ ":" ++ show x ++ acc

displaySystem :: Masks -> CardID -> FieldMap -> String
displaySystem masks cardID = formatFieldMap (Just cardID) . filterMasks masks

displayOwned :: Masks -> CardID -> FieldMap -> String
displayOwned masks cardID = formatFieldMap (Just cardID) . filterMasks masks

displayUnowned :: Masks -> CardID -> FieldMap -> String
displayUnowned masks cardID fm
  = case toEnum <$> Map.lookup Revealed fm of
      (Just True) -> formatFieldMap (Just cardID) . filterMasks masks $ fm
      _ -> formatFieldMap Nothing . filterMasks masks $ fm
