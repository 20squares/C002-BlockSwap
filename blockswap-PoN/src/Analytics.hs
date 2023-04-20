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

reporterGame Parameters{..}  paymentFunctionReporter = report reporterName payoutPoolParameter actionsGrievingProposer actionsMissingRequestProposer actionsMissingReplyProposer actionsReplyTimeout actionsWrongSignature actionsMissingRequestBuilder actionsMissingReplyBuilder actionsLowPayment actionsFaultAndKicking aggregateReportFunction penaltyValidator penaltyBuilder penaltyValidatorKicking verifyReport paymentFunctionReporter

--------------------------
-- 1. Equilibrium checking
--------------------------
{-
-- XXX
equilibriumXXX parameters strategy = evaluate (reporterGame parameters) strategy ctxt
 where
   ctxt = StochasticStatefulContext (pure ((),())) (\_ _ -> pure ())

-- printEquilibriumXXX parameters strategy = generateIsEq $ equilibriumXXX parameters strategy

-----------------
-- 2. Simulations
-----------------

-- XXX
simulateXXX Parameters{..} strategy = play game strategy 
--}
