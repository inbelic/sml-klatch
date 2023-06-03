module Internal.Cmds
  ( Cmd(..)
  , ok
  , invalid
  , order
  , target
  , concede
  , display
  ) where

-- Bindings that correspond to slatch/include/cmds.hrl and the client side
-- TODO: Could probably generate this and the other header file in Makefile

import Data.Word (Word8)
import Data.Char (chr)

-- WARNING: Ensure that the erlang server is updated if you changes these

-- General server cmds
newtype Cmd = Cmd
  { getCmd :: Word8
  }

ok :: Cmd 
ok = mkCmd 0

invalid :: Cmd
invalid = mkCmd 1

-- Playing (client cmds)
order :: Cmd
order = mkCmd 5

target :: Cmd
target = mkCmd 6

concede :: Cmd
concede = mkCmd 7

display :: Cmd
display = mkCmd 8


-- Helper to convert
mkCmd :: Int -> Cmd
mkCmd = Cmd . toEnum
