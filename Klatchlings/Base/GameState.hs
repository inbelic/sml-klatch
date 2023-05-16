module Base.GameState
  ( Game(..)
  , GameState(..)
  , peek
  , isStackEmpty
  , noAbilitiesTriggering
  ) where

import Base.Card (view)
import Base.Fields
import Base.History

import Internal.Types

peek :: Game -> CompiledWindows -> GameState
peek (Game stck hist crds) = GameState stck hist . view crds

isStackEmpty :: GameState -> Bool
isStackEmpty (GameState stck _ _) = null stck

noAbilitiesTriggering :: GameState -> Bool
noAbilitiesTriggering (GameState _ hist _)
  = null . current $ hist
