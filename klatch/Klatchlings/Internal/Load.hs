module Internal.Load
  ( LoadInfo(..)
  , CompiledWindows(..)
  , Mask(..)
  , Masks
  , CompiledMasks(..)
  , filterMasks
  , basicLoadInfo
  ) where

import Base.Fields

import Internal.Cut
import Internal.Types

import qualified Data.Map as Map (Map, filterWithKey)

-- All fields will default to not being shown and a mask will denote a
-- rule to whether we should show it our not. We slightly change our
-- intrepretation of a Filter in this instance. Hence, we can read the
-- following as, show Field if the other Fields of the card satisfy
-- the Filters. So the list of filters within a SINGLE mask are a series
-- of AND statements, whereas, the series of masks can are OR's so if any
-- Mask evaluates to True for a field then it will be shown
--
-- Examples:
--  Mask Zone [] -> Always show Zone
--
--  Mask SetID [ Filter Revealed (fromEnum True) Eq
--             , Filter Zone (fromEnum Hand) Eq
--             ]
--    -> Show the SetID if the Revealed field flag is set to True AND
--  in the Hand zone
--
--  [ Mask SetID [ Filter Revealed (fromEnum True) Eq
--               , Filter Zone (fromEnum Hand) Eq
--               ]
--  , Mask SetID [ Filter Zone (fromEnum Barracks) ]
--  ]
--    -> Show the SetID if (the Revealed field flag is set to True AND
--  in the Hand zone) OR if in the Barrack zone
data Mask = Mask Field (Selection Field)
  deriving Show
type Masks = [Mask]

-- We will compile the masks to ensure the same ordering of output of
-- fields among all parsing parties. The first element of the tuple
-- will be the masks for system owned cards, the middle for cards when
-- they are displayed to their owner and the third will be for cards
-- displayed to their opponent
newtype CompiledMasks = CompiledMasks
  { getMasks :: (Masks, Masks, Masks)
  }

data LoadInfo = LoadInfo
  { compiledWindows   :: CompiledWindows
  , compiledMasks     :: CompiledMasks
  }

-- Apply the masks over the given FieldMap and return the resulting FieldMap
-- with the Fields that satisfy their respective filters
filterMasks :: Masks -> FieldMap -> FieldMap
filterMasks masks fm = Map.filterWithKey (filterMasks' masks fm) fm
  where
    filterMasks' :: Masks -> FieldMap -> Field -> Int -> Bool
    filterMasks' masks fm fld _ = any (checkMask fm fld) masks

    checkMask :: FieldMap -> Field -> Mask -> Bool
    checkMask fm fld (Mask curFld selection)
      | fld /= curFld = False
      | otherwise = select' selection fm

-- An empty filter set means we will take all cards
basicFilterSet :: CompiledWindows
basicFilterSet = CWindows [ Window Owner $ Filtration []
                          , Window Phase  $ Filtration []
                          ]

basicMaskSet :: CompiledMasks
basicMaskSet = CompiledMasks
  ( [Mask Phase $ Selection [Constraint Phase Tr 0]]
  , []
  , []
  )

basicLoadInfo :: LoadInfo
basicLoadInfo = LoadInfo basicFilterSet basicMaskSet
