{-# LANGUAGE RecordWildCards #-}

module Payoffs where

import SupportFunctions
import Types

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Bool (Bool)

{-
Describes the payoffs for the different players
-}

-- Verify actions by reporter
-- NOTE if no report has been filed, we default to _Nothing_
verifyReport :: Eq b => State -> SubmitReport AgentPenalized b -> Maybe (ReportVerification AgentPenalized)
verifyReport State{..} report
  | report == NoReport = Nothing

-- Check preconditions 
preconditions state proposerAddr builderAddr slot
  | checkRegistered state proposerAddr == False = False -- ^ If not a registered validator, no fault on the validator's side
  | checkRegistered state proposerAddr == True && checkPayment state slot builderAddr == True = False -- ^ Registered validator, payment realized
  | checkRegistered state proposerAddr == True && checkPayment state slot builderAddr == False && checkDemand state slot == False = False -- ^ No payment realized for registered proposer but no demand -> all good
  | checkRegistered state proposerAddr == True && checkPayment state slot builderAddr == False && checkDemand state slot == True = True  -- ^ Possibly proposer fault

-- Check whether it is the proposer's fault (False == proposer not at fault, True == proposer at fault)
verifyProposerFault :: State -> ProposerAddr -> BuilderAddr -> SlotID ->  Bool
verifyProposerFault state@State{..} proposerAddr builderAddr slot
  | preconditions state proposerAddr builderAddr slot == False = False -- ^ proposer not at fault
  | preconditions state proposerAddr builderAddr slot == True && checkBlocksForSlot state slot == True = True  -- ^ proposer grieving the relays
  | preconditions state proposerAddr builderAddr slot == True && checkBlocksForSlot state slot == False && checkProposerRequest state slot == False = True  -- ^ proposer not having sent a request to at least one relay
  | preconditions state proposerAddr builderAddr slot == True && checkBlocksForSlot state slot == False && checkProposerRequest state slot == True && checkProposerReplied state slot == False = True  -- ^ proposer not having sent a request to at least one relay
  | otherwise = False -- ^ proposer not at faultslot  slot

-- Check whether it is the builder's fault (False == builder not at fault, True == builder at fault)
verifyBuilderFault :: State -> ProposerAddr -> BuilderAddr -> SlotID -> Bool
verifyBuilderFault state proposerAddr builderAddr slot
   | preconditions state proposerAddr builderAddr slot == True && checkBlocksForSlot state slot == False && checkProposerRequest state slot == True && checkProposerReplied state slot == True = True -- ^ If the proposer did everything right, but the payout pool still receives no money, it is the builder's fault

-- Reporter Payoff
-- This uses the defined payoff parameters
-- NOTE that we allow for penalities if reports are wrong
-- NOTE 
payoffReporter :: ReporterPayoffParameters -> Maybe (ReportVerification AgentPenalized) -> Payoff
payoffReporter ReporterPayoffParameters{..} report
  | report == Just (ReportCorrect Validator)       = reportCorrectValidator
  | report == Just (ReportCorrect Builder)         = reportCorrectBuilder
  | report == Just (ReportCorrect ValidatorKicked) = reportCorrectValidatorKicked
  | report == Just (ReportFalse Validator)         = reportFalseValidator
  | report == Just (ReportFalse Builder)           = reportFalseBuilder
  | report == Just (ReportFalse ValidatorKicked)   = reportFalseValidatorKicked
  | report == Nothing                              = 0
