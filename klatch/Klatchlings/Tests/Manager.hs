module Tests.Manager where

{-# LANGUAGE OverloadedStrings #-}

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Internal.Manager

-- ensure that our dressing and stripping of the GameID froma request and
-- response result in valid and equivalent messages
prop_comp :: Property
prop_comp =
  property $ do
    gID <- forAll $ GameID <$> Gen.integral (Range.linear 0 793759847159827432)
    let text = "stuff that is here"
        req = (gID, text)
    assert . (==) (Just req) . strip . uncurry dress $ req

managerGroup :: Group
managerGroup =
  Group "Test.Manager"
    [ ("Dressing and stripping requests", prop_comp)
    ]
