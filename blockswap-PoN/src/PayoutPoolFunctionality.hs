{-# LANGUAGE RecordWildCards#-}

module PayoutPoolFunctionality where

import Types

import OpenGames.Engine.Engine (Agent,Stochastic,uniformDist,pureAction,playDeterministically)


import Crem.BaseMachine
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)

{-
Contains the main logic of the payout pool as far as it concerns the reporter
-}

-- TODO Build the payout pool in an extensionable fashion
-- TODO We need to focus on the reporter's actions.

-- Can we do this in an aggregate state machine fashion?
