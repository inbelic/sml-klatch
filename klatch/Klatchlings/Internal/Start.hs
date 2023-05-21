module Internal.Start
  ( invokeGame
  ) where

import Base.History
import Base.Rules

import Internal.Engine (resolveStack)
import Internal.Types
import Internal.Load (LoadInfo(..), basicLoadInfo)
import Internal.Comms

import Control.Concurrent.Chan
import Control.Monad (void)
import qualified Data.Map as Map (fromList)

-- TODO
startGame :: String -> IO (LoadInfo, Game)
startGame _ = return (basicLoadInfo, Game [] begin cards)
  where
    cards = Map.fromList [(ruleCardID, baseRules)]

runGame :: LoadInfo -> Conn -> Game -> IO ()
runGame loadInfo ch game = void $ resolveStack loadInfo ch game

invokeGame :: Conn -> IO ()
invokeGame ch = do
  startInfo <- readChan (gameRead ch)
  (loadInfo, game) <- startGame startInfo
  runGame loadInfo ch game
