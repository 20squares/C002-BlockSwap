{-# LANGUAGE RecordWildCards #-}

module Payoffs where

import SupportFunctions
import Types

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)

{-
Describes the payoffs for the different players
-}

-- Verify actions by reporter
-- NOTE if no report has been filed, we default to _Nothing_
verifyReport
  :: (State, SlotID, ProposerAddr, BuilderAddr, SubmitReport Report) -> Maybe (ReportVerification AgentPenalized)
verifyReport (state, slot, proposerAddr, builderAddr, report) =
  case report of
     NoReport       -> Nothing
     SubmitReport r -> case penaltyType r of
       Validator ->
          if verifyProposerFault state proposerAddr builderAddr slot == True
              then Just $ ReportCorrect Validator
              else Just $ ReportFalse Validator
       Builder ->
          if verifyBuilderFault state proposerAddr builderAddr slot == True
              then Just $ ReportCorrect Builder
              else Just $ ReportFalse Builder
       ValidatorKicked  ->
          if verifyProposerKicking state proposerAddr == True
              then Just $ ReportCorrect ValidatorKicked
              else Just $ ReportFalse ValidatorKicked

-- Check preconditions
preconditions
  :: State -> ProposerAddr -> BuilderAddr -> SlotID -> Bool
preconditions state proposerAddr builderAddr slot
  | checkReportInterval state slot == False = False
  | checkRegistered state proposerAddr == False        = False -- ^ If not a registered validator, no fault on the validator's side
  | checkRegistered state proposerAddr == True
    && checkPayment state slot builderAddr == True     = False -- ^ Registered validator, payment realized
  | checkRegistered state proposerAddr == True
    && checkPayment state slot builderAddr == False
    && checkDemand state slot == False                 = False -- ^ No payment realized for registered proposer but no demand -> all good
  | checkRegistered state proposerAddr == True
    && checkPayment state slot builderAddr == False
    && checkDemand state slot == True                  = True  -- ^ Possibly proposer fault

-- Check whether it is the proposer's fault (False == proposer not at fault, True == proposer at fault)
verifyProposerFault
  :: State -> ProposerAddr -> BuilderAddr -> SlotID ->  Bool
verifyProposerFault state@State{..} proposerAddr builderAddr slot
  | preconditions state proposerAddr builderAddr slot == False = False -- ^ proposer not at fault
  | preconditions  state proposerAddr builderAddr slot == True
    && checkBlocksForSlot state slot == True                              = True  -- ^ proposer grieving the relays
  | preconditions state proposerAddr builderAddr slot == True
    && checkBlocksForSlot state slot == False
    && checkProposerRequest state slot == False                           = True  -- ^ proposer not having sent a request to at least one relay
  | preconditions state proposerAddr builderAddr slot == True
    && checkBlocksForSlot state slot == False
    && checkProposerRequest state slot == True
    && checkProposerReplied state slot == False                           = True  -- ^ proposer not having sent a request to at least one relay
  | otherwise                                                             = False -- ^ proposer not at faultslot  slot

-- Check whether it is the builder's fault (False == builder not at fault, True == builder at fault)
verifyBuilderFault
  :: State -> ProposerAddr -> BuilderAddr -> SlotID -> Bool
verifyBuilderFault state proposerAddr builderAddr slot
   | verifyProposerFault state proposerAddr builderAddr slot == False
     && checkPayment state slot builderAddr == False                             = True -- ^ If the proposer did everything right, but the payout pool still receives no money, it is the builder's fault
   | verifyProposerFault state proposerAddr builderAddr slot == False
     && checkBuilderPayment state slot builderAddr == False                      = True -- ^ If the proposer behaved correctly and the payoutpool receives too little money, it is the builder's fault
   | otherwise                                                                   = False

-- Kick proposer for violating conditions (False == proposer not to be kicked; True == builder to be kicked)
-- TODO: also check the time dimension; should this happen for current slots or later slots
-- TODO: This needs to be checked
verifyProposerKicking
  :: State -> ProposerAddr -> Bool
verifyProposerKicking State{..} proposerAddr
  | stakes < 32 = True
  | status == ProposerExited = True
  | otherwise = False
  where
    stakes = proposerStake stateOnChain M.! proposerAddr
    status = proposerStatus statePoNOnChain M.! proposerAddr 

-- Reporter Payoff
-- This uses the defined payoff parameters
-- NOTE that we allow for penalities if reports are wrong
payoffReporter
  :: ReporterPayoffParameters -> Maybe (ReportVerification AgentPenalized) -> Payoff
payoffReporter ReporterPayoffParameters{..} report
  | report == Just (ReportCorrect Validator)       = reportCorrectValidator
  | report == Just (ReportCorrect Builder)         = reportCorrectBuilder
  | report == Just (ReportCorrect ValidatorKicked) = reportCorrectValidatorKicked
  | report == Just (ReportFalse Validator)         = reportFalseValidator
  | report == Just (ReportFalse Builder)           = reportFalseBuilder
  | report == Just (ReportFalse ValidatorKicked)   = reportFalseValidatorKicked
  | report == Nothing                              = 0
