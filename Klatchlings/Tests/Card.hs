module Tests.Card
  ( cardGroup
  ) where

{-# LANGUAGE OverloadedStrings #-}

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

cardGroup :: Group
cardGroup =
  Group "Tests.Card"
    []
