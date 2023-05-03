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
-- TODO Build the payout pool in an extensionable fashion: We do this by building the functionality in terms of machines that get aggregated
-- TODO We focus on the reporter's actions.
-}

-- Provides the interface between the verification of a reporter's report and the payout pool
reporterToPayoutPool :: ReporterAddr -> (State, SubmitReport Report) -> State
reporterToPayoutPool addr (s, r) =
  case r of
    NoReport        -> s
    SubmitReport r' -> reportToPayoutPool addr r' s

-- Change state given an actual report
reportToPayoutPool :: ReporterAddr -> Report -> State -> State
reportToPayoutPool reporterAddr report' s
 | preconditions reporterAddr report' s == False = s
 | preconditions reporterAddr report' s == True && report'._penaltyType == ValidatorKicked = kickProposer report' s
 | preconditions reporterAddr report' s == True && report'._penaltyType == Validator = penalizeProposer reporterAddr report' s
 | preconditions reporterAddr report' s == True && report'._penaltyType == Builder = penalizeBuilder reporterAddr report' s

-- Kick proposer
-- NOTE this is incomplete; to be augmented when proposer and builder are addressed
kickProposer :: Report -> State -> State
kickProposer r s =
   over (statePoNOnChain % proposerStatus ) (M.update (\_ -> Just ProposerKicked) (r._proposer))  s

-- Penalize proposer
-- NOTE this is incomplete; to be augmented when proposer and builder are addressed
penalizeProposer :: ReporterAddr -> Report -> State -> State
penalizeProposer addr r s = slashProposer addr r $ reportProposer r s
 where
    -- Report the proposer as being penalized
    -- NOTE this is incomplete; to be augmented when proposer and builder are addressed
    reportProposer :: Report -> State ->  State
    reportProposer r s =
      let proposer' = s._stateOnChain._payoutPool._proposerRegistry M.! r._proposer 
          in if proposer'._reportCount + 1 >= s._stateOnChain._payoutPool._kickThreshold
                then kickProposer r s
                else over (stateOnChain % payoutPool % proposerRegistry) (M.adjust (over reportCount (+1)) r._proposer) s
    -- Slashes the proposer and sends amount to reporter
    -- NOTE we focus on the payment to reporter part; adjustments to proposer tbd
    -- NOTE this is incomplete; to be augmented when proposer and builder are addressed
    slashProposer :: ReporterAddr -> Report -> State -> State
    slashProposer addr r s =
      over (stateOnChain % payoutPool % reporterRegistry) (M.adjust (over rewards (+ (r._amount))) addr) s

-- Penalize the builder
-- NOTE this is incomplete; to be augmented when proposer and builder are addressed
penalizeBuilder :: ReporterAddr -> Report -> State -> State
penalizeBuilder addr r s = slashBuilder addr r s
  where
    -- NOTE we focus on the payment to reporter part; adjustments to proposer tbd
    -- NOTE this is incomplete; to be augmented when proposer and builder are addressed
    slashBuilder addr r s = 
      over (stateOnChain % payoutPool % reporterRegistry) (M.adjust (over rewards (+ (r._amount))) addr) s

-- Registers a reporter; if already existing does nothing
registerReporter :: ReporterAddr -> State -> State
registerReporter addr s =
  case M.lookup addr s._stateOnChain._payoutPool._reporterRegistry of
    Just r  -> s
    Nothing -> over (stateOnChain % payoutPool % reporterRegistry) (M.insert addr reporter) s 
  where
    reporter = Reporter
       { _rewards           = 0
       , _isActive          = True
       , _isRageQuitted     = False
       , _lastReportedBlock = Nothing
       }

-- Rage quits a reporter; if non-existing does nothing
-- NOTE: deal with claims being withdrawn
rageQuitReporter :: ReporterAddr -> State -> State
rageQuitReporter addr s =
  case M.lookup addr s._stateOnChain._payoutPool._reporterRegistry of
    Nothing -> s
    Just r  -> case _lastReportedBlock r of
      Nothing                 -> s
      Just lastBlockReported  -> case lastBlockReported + s._stateOnChain._payoutPool._reporterPayoutDelay < s._stateOnChain._block of
        False -> s
        True  -> over (stateOnChain % payoutPool % reporterRegistry) (M.adjust (set isRageQuitted True) addr) s

-- Claim reporter rewards
-- NOTE: deal with claims being withdrawn
claimReporterRewards :: ReporterAddr -> State -> (ETH,State)
claimReporterRewards addr s =
  case M.lookup addr s._stateOnChain._payoutPool._reporterRegistry of
    Nothing -> (0,s)
    Just r  ->
      case _rewards r > 0 of
        False -> (0,s) -- ^ No funds to withdraw
        True  -> case _lastReportedBlock r of
          Nothing -> (0,s) -- ^ No report yet
          Just block -> case block + s._stateOnChain._payoutPool._reporterPayoutDelay <= s._stateOnChain._block of
            False -> (0,s) -- ^ Too early to withdraw funds
            True  -> (_rewards r, adjustBalancePayoutPool r setRewardsToZero)
  where
    setRewardsToZero = over (stateOnChain % payoutPool % reporterRegistry) (M.adjust (set rewards 0) addr) s
    adjustBalancePayoutPool r' s' = over (stateOnChain % payoutPool % maintenanceBalance) (\balance -> balance - _rewards r') s'


-- Checks that the relevant pre-conditions are fulfilled for a report to be field
-- NOTE Conditions are not complete wrt to builder and proposer
preconditions :: ReporterAddr -> Report -> State -> Bool
preconditions reporterAddr report' s = and 
  [ s._stateOnChain._block < report'._blockId + s._stateOnChain._payoutPool._payoutCycleLength
  , (s._stateOnChain._payoutPool._reporterRegistry M.! reporterAddr)._isActive  == True
  , (s._stateOnChain._payoutPool._reporterRegistry M.! reporterAddr)._isRageQuitted == False
  , report'._slotId > s._stateOnChain._payoutPool._deploymentEpoch * 32
  , M.lookup report'._slotId s._stateOnChain._payoutPool._reportsSlotsInUse == Nothing
  ]
 
 
