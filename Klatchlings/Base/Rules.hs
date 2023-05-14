module Base.Rules where

import Base.Card
import Base.CardState
import Base.Fields
import Base.GameState

import Internal.Types

import qualified Data.Map as Map (fromList)

ruleCardID :: CardID
ruleCardID = CardID 0

baseRules :: Card
baseRules =
  discardAlteration (set Phase (fromEnum Morning))
  . discardAlteration (set Owner (fromEnum System))
  . foldr (discardAlteration . equip) create
  $ abltys
  where
    abltys = [incrementPhase]

incrementPhase :: Ability
incrementPhase = Ability OnTrigger trg grd rslvs rulesCard
  where
    trg = Trigger $ \_ gs -> isStackEmpty gs && noAbilitiesTriggering gs
    grd = Guard $ \_ _ gs -> isStackEmpty gs && noAbilitiesTriggering gs
    rslv = Resolve $ \_ _ (GameState _ _ cs) ->
            case toEnum . check ruleCardID Phase (fromEnum Night) $ cs of
              Night -> set Phase $ fromEnum Morning
              phase -> set Phase . fromEnum . succ $ phase

    rslvs = Map.fromList [(TargetID 0, rslv)]

    rulesCard = Targeting $ \_ _ -> [(TargetID 0, Given ruleCardID)]
