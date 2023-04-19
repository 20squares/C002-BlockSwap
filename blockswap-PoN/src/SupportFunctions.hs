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
-- check register status of proposer (False == not registered, True == registered)
checkRegistered :: State -> Bool
checkRegistered (State StateOnChain{..} StatePoNOnChain{..} _) =
   let proposer = proposerForSlot M.! slotId
       status   = proposerStatus M.! proposer
       in status == ProposerRegistered

-- check whether payment was received (False == no payment, True == payment)
checkPayment :: State -> Bool
checkPayment (State StateOnChain{..} StatePoNOnChain{..} _) =
  let filteredMap = M.filterWithKey (\(slotId',_) _ -> slotId' == slotId) paidInSlot
      payment = head $ M.elems filteredMap  -- FIXME Hack
      in payment > 0

-- check whether there was demand (False == no demand, True == demand)
checkDemand :: State -> Bool
checkDemand (State StateOnChain{..} StatePoNOnChain{..} (_,auction)) =
  let bids = auction M.! slotId
      in if bids == []
            then False
            else True

-- Check whether blocks where actually signed (False == not signed, True == signed )
-- FIXME we are using this for check whether a proposer went outside of the relays
checkBlocksForSlot :: State -> Bool
checkBlocksForSlot (State StateOnChain{..} _ _ ) =
  if M.lookup slotId signedBlocks == Nothing
     then False
     else True

-- Check proposer request (False == no request sent; True == request sent)
checkProposerRequest :: State -> Bool
checkProposerRequest (State StateOnChain{..} _ (relays,_)) =
  or $ fmap (\relay -> request relay M.! slotId) relays

-- Check proposer does not reply (False == not replied; True == replied)
-- TODO Check: is this using the correct function?
-- TODO Check: it seems that the different cases outlined in the doc cannot be distinguished
checkProposerReplied :: State -> Bool
checkProposerReplied (State StateOnChain{..} _ (relays,_)) =
  or $ fmap (\relay -> not $ null $ receive relay M.! slotId) relays
  -- ^ Check for each relay whether the returned string is empty; negate and check whether at least one string is non-empty
  
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
