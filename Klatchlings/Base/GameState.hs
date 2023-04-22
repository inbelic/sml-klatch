module Base.GameState
  ( Game(..)
  , GameState(..)
  , peek
  ) where

import Base.Card (view)
import Base.Fields
import Internal.Filters (CompiledFilters)
import Internal.Types

peek :: Game -> CompiledFilters -> GameState
peek (Game stck hist crds) = GameState stck hist . view crds
