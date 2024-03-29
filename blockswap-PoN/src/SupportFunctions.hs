{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}

module SupportFunctions where

import Types

import OpenGames.Engine.Engine (Agent,Stochastic,uniformDist,pureAction,playDeterministically)

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Maybe


{--
Contains basic auxiliary functionality needed for model
-}

-- Check that the block to be reported is sufficiently distant relative to the current block
-- (False == Slot cannot (yet) be reported; True == slot can be reported) 
checkReportInterval :: State -> SlotID -> Bool
checkReportInterval State{..} slot =
  slot + _stateOnChain._payoutPool._payoutCycleLength < _stateOnChain._slotId

-- check register status of proposer (False == not registered, True == registered)
-- TODO: In principle this has to be checked for the time at which the slot was proposed
checkRegistered :: State -> ProposerAddr -> Bool
checkRegistered (State StateOnChain{..} StatePoNOnChain{..} _) proposer =
   let status   = _proposerStatus M.! proposer
       in status == ProposerRegistered

-- check whether payment was received (False == no payment, True == payment)
checkPayment :: State -> SlotID -> BuilderAddr -> Bool
checkPayment (State StateOnChain{..} StatePoNOnChain{..} _) slot builder =
  let payment = M.lookup (slot,builder) _paidInSlot
      in isJust payment

-- check whether there was demand (False == no demand, True == demand)
checkDemand :: State -> SlotID -> Bool
checkDemand (State StateOnChain{..} StatePoNOnChain{..} (_,auction)) slot =
  let bids = auction M.! slot
      in bids /= []

-- Check whether blocks where actually signed (False == not signed, True == signed )
-- FIXME we are using this for check whether a proposer went outside of the relays
checkBlocksForSlot :: State -> SlotID -> Bool
checkBlocksForSlot (State StateOnChain{..} _ _ ) slot =
  isJust (M.lookup slot _signedBlocks)

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
  let actualPayment   = M.lookup (slot,builder') _paidInSlot
      auctionForBlock = auction M.! slot
      bidsByBuilder   = head $ filter (\b -> b.builder == builder') auctionForBlock -- ^ TODO we make the assumption that we take the head of the list
      promisedPayment = promise bidsByBuilder
      in case actualPayment of
            Nothing       -> False
            Just payment' -> payment' >= promisedPayment

-- aggregate the different reporting information
aggregateReportFunction (slotInPast, registeredProposer, paymentReceived,demand, reportGrieving, reportMissingRequestProposer,reportMissingReplyProposer,reportReplyTimeOut, reportSignature, reportMissingRequestBuilder, reportMissingReplyBuilder, reportLowPayment)
  | not slotInPast || not registeredProposer || not demand                                                          = NoPenalty
  | slotInPast && registeredProposer && not paymentReceived && demand && reportGrieving == Penalty Grieving                        = Penalty Grieving
  | slotInPast && registeredProposer && not paymentReceived && demand && reportMissingRequestProposer == Penalty ProposerNoRequest = Penalty ProposerNoRequest
  | slotInPast && registeredProposer && not paymentReceived && demand && reportMissingReplyProposer == Penalty ProposerNotReplied  = Penalty ProposerNotReplied
  | slotInPast && registeredProposer && not paymentReceived && demand && reportReplyTimeOut == Penalty NotWithinTime               = Penalty NotWithinTime
  | slotInPast && registeredProposer && not paymentReceived && demand && reportSignature == Penalty NotVerified                    = Penalty NotVerified
  | slotInPast && registeredProposer && not paymentReceived && demand && reportMissingRequestBuilder == Penalty BuilderNoRequest   = Penalty BuilderNoRequest
  | slotInPast && registeredProposer && not paymentReceived && demand && reportMissingReplyBuilder == Penalty BuilderNotReplied    = Penalty BuilderNotReplied
  | slotInPast && registeredProposer && paymentReceived && demand && reportLowPayment == Penalty LowPayment                        = Penalty LowPayment
  | otherwise                                                                                                                                                = NoPenalty

-- Pattern match the different penalties with the report
-- TODO: We fix the penalty parameter here at 0
matchPenaltyForReport slotId addrProposer addrBuilder x
  | x == Grieving           = SubmitReport (report Validator)
  | x == ProposerNoRequest  = SubmitReport (report Validator)
  | x == ProposerNotReplied = SubmitReport (report Validator)
  | x == NotWithinTime      = SubmitReport (report Validator)
  | x == NotVerified        = SubmitReport (report Validator)
  | x == BuilderNoRequest   = SubmitReport (report Builder)
  | x == BuilderNotReplied  = SubmitReport (report Builder)
  | x == LowPayment         = SubmitReport (report Builder)
  | x == Kicked             = SubmitReport (report ValidatorKicked)
  where
    report x = Report
        { _proposer    = addrProposer
        , _builder     = addrBuilder
        , _amount      = 0
        , _slotId      = slotId
        , _blockId     = 0
        , _penaltyType = x
        }

-- Forward report if verified else no-report
forwardReport :: (ReportVerification AgentPenalized, SubmitReport Report) -> SubmitReport Report
forwardReport (verified, report) =
  case verified of
    ReportCorrect _ -> report
    ReportFalse   _ -> NoReport

-- Determine if gas fees if a report is submitted
costsSubmittingReport :: Eq a => SubmissionCosts -> SubmitReport a -> ETH
costsSubmittingReport costs report =
  if report == NoReport
     then 0
     else costs
