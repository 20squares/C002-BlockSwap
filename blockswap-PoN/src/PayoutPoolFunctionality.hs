{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}

module PayoutPoolFunctionality where

import Types

import OpenGames.Engine.Engine (Agent,Stochastic,uniformDist,pureAction,playDeterministically)

import Crem.BaseMachine(BaseMachine(..),ActionResult(..))
import Crem.StateMachine
import Crem.Topology(Topology(..))
import Data.Kind
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Optics

{-
Contains the main logic of the payout pool as far as it concerns the reporter
-}

-- TODO Build the payout pool in an extensionable fashion: We do this by building the functionality in terms of machines that get aggregated
-- TODO We focus on the reporter's actions.

-- reportToPayoutPool :: Report -> State -> State
reportToPayoutPool reporter' report' s
 | preconditions reporter' report' s == False = s
 | preconditions reporter' report' s == True && report'._penaltyType == ValidatorKicked = kickProposer report' s

-- NOTE this is incomplete; to be augmented when proposer and builder are addressed
kickProposer :: Report -> State -> State
kickProposer r s =
   over (statePoNOnChain % proposerStatus ) (M.update (\_ -> Just ProposerKicked) (r._proposer))  s

-- NOTE this is incomplete; to be augmented when proposer and builder are addressed
penalizeProposer :: Report -> State -> State
penalizeProposer r s = undefined

-- TODO this is incomplete; to be augmented when proposer and builder are addressed
reportProposer :: Report -> State -> State
reportProposer r s =
  let proposer' = s._stateOnChain._payoutPool._proposerRegistry M.! r._proposer 
      in if proposer'._reportCount + 1 >= s._stateOnChain._payoutPool._kickThreshold
            then kickProposer r s
            else s

-- NOTE Conditions are not complete wrt to builder and proposer
preconditions :: Reporter -> Report -> State -> Bool
preconditions reporter' report' s = and 
  [ s._stateOnChain._block < report'._blockId + s._stateOnChain._payoutPool._payoutCycleLength
  , reporter'.isActive == True
  , reporter'.isRageQuitted == False
  , report'._slotId > s._stateOnChain._payoutPool._deploymentEpoch * 32
  , M.lookup report'._slotId s._stateOnChain._payoutPool._reportsSlotsInUse == Nothing
  ]


 
