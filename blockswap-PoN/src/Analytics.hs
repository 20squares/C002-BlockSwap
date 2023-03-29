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

import SupportFunctions
import Model
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

game = undefined 

--------------------------
-- 1. Equilibrium checking
--------------------------

-- XXX
equilibriumXXX Parameters{..} strategy = evaluate game strategy ctxt
 where
   ctxt = StochasticStatefulContext (pure ((),())) (\_ _ -> pure ())

-- printEquilibriumXXX parameters strategy = generateIsEq $ equilibriumXXX parameters strategy

-----------------
-- 2. Simulations
-----------------

-- XXX
simulateXXX Parameters{..} strategy = play game strategy 

