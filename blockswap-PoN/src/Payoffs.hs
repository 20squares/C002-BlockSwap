{-# LANGUAGE RecordWildCards #-}

module Payoffs where

import Types

{-
Describes the payoffs for the different players
-}


-- Verify actions by reporter
-- NOTE we assume only when 
-- verifyReport :: State -> SubmitReport AgentPenalized -> ReportVerification AgentPenalized


-- Reporter
payoffReporter :: ReporterPayoffParameters -> ReportVerification AgentPenalized -> Payoff
payoffReporter ReporterPayoffParameters{..} report
  | report == ReportCorrect Validator       = reportCorrectValidator
  | report == ReportCorrect Builder         = reportCorrectBuilder
  | report == ReportCorrect ValidatorKicked = reportCorrectValidatorKicked
  | report == ReportFalse Validator         = reportFalseValidator
  | report == ReportFalse Builder           = reportFalseBuilder
  | report == ReportFalse ValidatorKicked   = reportFalseValidatorKicked
