{-# LANGUAGE RecordWildCards#-}

module SupportFunctions where

import Types

import OpenGames.Engine.Engine (Agent,Stochastic,uniformDist,pureAction,playDeterministically)

{--
Contains basic auxiliary functionality needed for model
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

-- aggregate the different reporting information
aggregateReportFunction (registeredProposer, missedPayment,demand, reportGrieving, reportMissingRequestProposer,reportMissingReplyProposer,reportReplyTimeOut, reportSignature, reportMissingRequestBuilder, reportMissingReplyBuilder, reportLowPayment, reportProposerFaultAndKicking)
  | registeredProposer == False || missedPayment == False || demand == False = NoPenalty
  | registeredProposer == True && missedPayment == True && demand == True && reportGrieving == Penalty Grieving = Penalty Grieving
  | registeredProposer == True && missedPayment == True && demand == True && reportMissingRequestProposer == Penalty ProposerNoRequest = Penalty ProposerNoRequest
  | registeredProposer == True && missedPayment == True && demand == True && reportMissingReplyProposer == Penalty ProposerNotReplied = Penalty ProposerNotReplied
  | registeredProposer == True && missedPayment == True && demand == True && reportReplyTimeOut == Penalty NotWithinTime = Penalty NotWithinTime
  | registeredProposer == True && missedPayment == True && demand == True && reportSignature == Penalty NotVerified = Penalty NotVerified
  | registeredProposer == True && missedPayment == True && demand == True && reportMissingRequestBuilder == Penalty BuilderNoRequest = Penalty BuilderNoRequest
  | registeredProposer == True && missedPayment == True && demand == True && reportMissingReplyBuilder == Penalty BuilderNotReplied = Penalty BuilderNotReplied
  | registeredProposer == True && missedPayment == False && demand == True && reportLowPayment == Penalty LowPayment = Penalty LowPayment
  | registeredProposer == True && reportProposerFaultAndKicking == Penalty Kicked = Penalty Kicked
