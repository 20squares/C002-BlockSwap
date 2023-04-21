{-# LANGUAGE RecordWildCards#-}

module SupportFunctions where

import Types

import OpenGames.Engine.Engine (Agent,Stochastic,uniformDist,pureAction,playDeterministically)

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)


{--
Contains basic auxiliary functionality needed for model
-}

-- Check that the block to be reported is sufficiently distant relative to the current block
-- (False == Slot cannot (yet) be reported; True == slot can be reported) 
checkReportInterval :: PayoutPool -> State -> SlotID -> Bool
checkReportInterval PayoutPool{..} State{..} slot =
  slot + cycleLength < slotId stateOnChain

-- check register status of proposer (False == not registered, True == registered)
-- TODO: In principle this has to be checked for the time at which the slot was proposed
checkRegistered :: State -> ProposerAddr -> Bool
checkRegistered (State StateOnChain{..} StatePoNOnChain{..} _) proposer =
   let status   = proposerStatus M.! proposer
       in status == ProposerRegistered

-- check whether payment was received (False == no payment, True == payment)
checkPayment :: State -> SlotID -> BuilderAddr -> Bool
checkPayment (State StateOnChain{..} StatePoNOnChain{..} _) slot builder =
  let payment = paidInSlot M.! (slot,builder)
      in payment > 0

-- check whether there was demand (False == no demand, True == demand)
checkDemand :: State -> SlotID -> Bool
checkDemand (State StateOnChain{..} StatePoNOnChain{..} (_,auction)) slot =
  let bids = auction M.! slot
      in if bids == []
            then False
            else True

-- Check whether blocks where actually signed (False == not signed, True == signed )
-- FIXME we are using this for check whether a proposer went outside of the relays
checkBlocksForSlot :: State -> SlotID -> Bool
checkBlocksForSlot (State StateOnChain{..} _ _ ) slot =
  if M.lookup slot signedBlocks == Nothing
     then False
     else True

-- Check proposer request (False == no request sent; True == request sent)
checkProposerRequest :: State -> SlotID -> Bool
checkProposerRequest (State StateOnChain{..} _ (relays,_)) slot =
  or $ fmap (\relay -> request relay M.! slot) relays

-- Check proposer does not reply (False == not replied; True == replied)
-- TODO Check: is this using the correct function?
-- TODO Check: it seems that the different cases for the proposer outlined in the doc cannot be distinguished
-- TODO Check: it seems that the different cases for the builder outlined in the doc cannot be distinguished
checkProposerReplied :: State -> SlotID -> Bool
checkProposerReplied (State StateOnChain{..} _ (relays,_)) slot=
  or $ fmap (\relay -> not $ null $ receive relay M.! slot) relays
  -- ^ Check for each relay whether the returned string is empty; negate and check whether at least one string is non-empty

-- Check builder does not pay as promised (False == paid too little; True == paid enough)
checkBuilderPayment (State StateOnChain{..} StatePoNOnChain{..} (relays,auction)) slot builder' =
  let actualPayment   = paidInSlot M.! (slot,builder')
      auctionForBlock = auction M.! slot
      bidsByBuilder   = head $ filter (\b -> builder b == builder') auctionForBlock -- ^ TODO we make the assumption that we take the head of the list
      promisedPayment = promise bidsByBuilder
      in actualPayment >= promisedPayment

-- aggregate the different reporting information
aggregateReportFunction (slotInPast, registeredProposer, missedPayment,demand, reportGrieving, reportMissingRequestProposer,reportMissingReplyProposer,reportReplyTimeOut, reportSignature, reportMissingRequestBuilder, reportMissingReplyBuilder, reportLowPayment, reportProposerFaultAndKicking)
  | slotInPast == False || registeredProposer == False || missedPayment == False || demand == False = NoPenalty
  | slotInPast == True && registeredProposer == True && missedPayment == True && demand == True && reportGrieving == Penalty Grieving = Penalty Grieving
  | slotInPast == True && registeredProposer == True && missedPayment == True && demand == True && reportMissingRequestProposer == Penalty ProposerNoRequest = Penalty ProposerNoRequest
  | slotInPast == True && registeredProposer == True && missedPayment == True && demand == True && reportMissingReplyProposer == Penalty ProposerNotReplied = Penalty ProposerNotReplied
  | slotInPast == True && registeredProposer == True && missedPayment == True && demand == True && reportReplyTimeOut == Penalty NotWithinTime = Penalty NotWithinTime
  | slotInPast == True && registeredProposer == True && missedPayment == True && demand == True && reportSignature == Penalty NotVerified = Penalty NotVerified
  | slotInPast == True && registeredProposer == True && missedPayment == True && demand == True && reportMissingRequestBuilder == Penalty BuilderNoRequest = Penalty BuilderNoRequest
  | slotInPast == True && registeredProposer == True && missedPayment == True && demand == True && reportMissingReplyBuilder == Penalty BuilderNotReplied = Penalty BuilderNotReplied
  | slotInPast == True && registeredProposer == True && missedPayment == False && demand == True && reportLowPayment == Penalty LowPayment = Penalty LowPayment
  | slotInPast == True && registeredProposer == True && reportProposerFaultAndKicking == Penalty Kicked = Penalty Kicked

-- Pattern match the different penalties with the report
-- TODO: We fix the penalty parameter here at 0
matchPenaltyForReport x
  | x == Grieving           = SubmitReport Validator 0
  | x == ProposerNoRequest  =  SubmitReport Validator 0
  | x == ProposerNotReplied =  SubmitReport Validator 0
  | x == NotWithinTime      =  SubmitReport Validator 0
  | x == NotVerified        =  SubmitReport Validator 0
  | x == BuilderNoRequest   =  SubmitReport Builder 0
  | x == BuilderNotReplied  =  SubmitReport Builder 0
  | x == LowPayment         =  SubmitReport Builder 0
  | x == Kicked             =  SubmitReport ValidatorKicked 0
