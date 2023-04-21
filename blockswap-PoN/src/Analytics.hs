{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Analytics
   where

import ActionSpaces
import Model
import Payoffs
import SupportFunctions
import Types

import OpenGames.Engine.Engine hiding (Payoff)
import OpenGames.Preprocessor


import qualified Control.Monad.State  as ST
import qualified Numeric.Probability.Distribution as P

{-
Contains the basic analytics to be executed
There are two type of analyses:
1. (Bayesian) Nash eq. checks
2. Simulations
-}

reporterGame Parameters{..}  = report reporterName payoutPoolParameter actionsGrievingProposer actionsMissingRequestProposer actionsMissingReplyProposer actionsReplyTimeout actionsWrongSignature actionsMissingRequestBuilder actionsMissingReplyBuilder actionsLowPayment actionsFaultAndKicking aggregateReportFunction penaltyValidator penaltyBuilder penaltyValidatorKicking verifyReport payoffReporter reporterPayoffParameters

--------------------------
-- 1. Equilibrium checking
--------------------------

-- Equilibrium notiton for reporter game
equilibriumReporterGame parameters@Parameters{..} strategy = evaluate (reporterGame parameters) strategy ctxt
 where
   ContextParameters{..} = contextParameters
   ctxt = StochasticStatefulContext (pure ((),(ctxState, ctxSlotId, ctxProposerAddr, ctxBuilderAddr))) (\_ _ -> pure ())

printEquilibriumReporterGame parameters strategy = generateOutput $ equilibriumReporterGame parameters strategy

-----------------
-- 2. Simulations
-----------------

-- Simulate reporter game
simulateReporterGame parameters strategy = play (reporterGame parameters) strategy 

