module Main where

import Base.Card
import Base.Display
import Base.GameState
import Base.History
import Base.Rules

import Internal.Filters
import Internal.Engine
import Internal.Types
import Internal.Load (LoadInfo(..), basicLoadInfo)

import qualified Data.Map as Map (fromList)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent (forkIO)

harness :: Chan String -> IO ()
harness ch = do
  req <- readChan ch
  print req
  rep <- getLine
  writeChan ch rep
  harness ch

invokeGame :: Chan String -> Game -> IO ()
invokeGame ch game = do
  game' <- resolveStack basicLoadInfo ch game
  return ()

main :: IO ()
main = do
  let cards = Map.fromList [(ruleCardID, baseRules)]
      game = Game [] begin cards
  print . view cards . compiledFilters $ basicLoadInfo
  print . displayStateCallback basicLoadInfo
        . peek game
        . compiledFilters $ basicLoadInfo
  ch <- newChan
  forkIO (invokeGame ch game)
  harness ch
