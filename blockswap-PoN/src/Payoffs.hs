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
verifyReport :: State -> SubmitReport AgentPenalized -> Maybe (ReportVerification AgentPenalized)
verifyReport State{..} report
  | report == NoReport = Nothing

-- Check whether it is the proposer's fault (True == proposer at fault, False == proposer not at fault)
verifyProposerFault :: State -> Bool
verifyProposerFault state@State{..}
  | preconditions state == False = False -- ^ proposer not at fault
  | preconditions state == True && checkBlocksForSlot state == True = True  -- ^ proposer grieving the relays
  | preconditions state == True && checkBlocksForSlot state == False && checkProposerRequest state == False = True  -- ^ proposer not having sent a request to at least one relay
  | preconditions state == True && checkBlocksForSlot state == False && checkProposerRequest state == True && checkProposerReplied state == False = True  -- ^ proposer not having sent a request to at least one relay
  | otherwise = False -- ^ proposer not at fault
  where
    preconditions state
      | checkRegistered state == False = False -- ^ If not a registered validator, no fault on the validator's side
      | checkRegistered state == True && checkPayment state == True = False -- ^ Registered validator, payment realized
      | checkRegistered state == True && checkPayment state == False && checkDemand state == False = False -- ^ No payment realized for registered proposer but no demand -> all good
      | checkRegistered state == True && checkPayment state == False && checkDemand state == True = True  -- ^ Possibly proposer fault



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
