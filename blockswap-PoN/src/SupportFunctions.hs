{-# LANGUAGE RecordWildCards#-}

module SupportFunctions where

import Types

import OpenGames.Engine.Engine (Agent,Stochastic,uniformDist,pureAction,playDeterministically)

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)


{--
Contains basic auxiliary functionality needed for model
-}

-- TODO
-- check register status of proposer
checkRegistered :: State -> Bool
checkRegistered (State StateOnChain{..} StatePoNOnChain{..} _) =
   let proposer = proposerForSlot M.! slotId
       status   = proposerStatus M.! proposer
       in status == ProposerRegistered

-- check whether payment was received
checkMissedPayment :: State -> Bool
checkMissedPayment (State StateOnChain{..} StatePoNOnChain{..} _) =
  let filteredMap = M.filterWithKey (\(slotId',_) _ -> slotId' == slotId) paidInSlot
      payment = head $ M.elems filteredMap  -- FIXME Hack
      in payment > 0

-- check whether there was demand
checkDemand :: State -> Bool
checkDemand (State StateOnChain{..} StatePoNOnChain{..} (_,auction)) =
  let bids = auction M.! slotId
      in if bids == []
            then False
            else True

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
