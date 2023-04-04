{-# LANGUAGE RecordWildCards#-}

module SupportFunctions where

import Types

import OpenGames.Engine.Engine (Agent,Stochastic,uniformDist,pureAction,playDeterministically)

{--
Contains basic functionality needed for model
-}


-- check register status of proposer
checkRegistered :: SlotStatus -> Bool
checkRegistered = proposerRegistered

-- check whether payment was received
checkMissedPayment :: SlotStatus -> Bool
checkMissedPayment SlotStatus{..} =
  if paymentReceived == Nothing then False else True
  
-- check whether there was demand
checkDemand :: SlotStatus -> Bool
checkDemand SlotStatus{..} =
  if proposerAction == NoBlockRequested then False else True
