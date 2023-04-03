{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module Components
  where

import SupportFunctions
import Types

import OpenGames.Engine.Engine
import OpenGames.Preprocessor

{-
We build a reporter as a composite of different games.
This should make it easier to extend and change things.
In principle, this also allows for multiple reporter "types"
-}

-------------------
-- 1 Reporter roles
-------------------

-- Was the block missed? And was there demand?
reportMissingBlock name  = [opengame|

    inputs    :  slotStatus ;
    feedback  :   ;

    :---------------------------:
    inputs    :  slotStatus  ;
    feedback  :   ;
    operation :  dependentDecision name (const [NotMissed,Missed]) ;
    outputs   :  missedBlock ;
    returns   :  0 ;
    // Turn to payoffs later

    inputs    :  missedBlock, slotStatus  ;
    feedback  :   ;
    operation :  dependentDecision name (const [NoDemand,Demand]) ;
    outputs   :  demand ;
    returns   :  0 ;
    // Turn to payoffs later
    :---------------------------:

    outputs   :  missedBlock,demand ;
    returns   :   ;
  |]



-- Report registered proposer receiving payment
-- Report proposer if registered proposer and payment received but no block requested
-- MAYBE: Filter through the information; so that at each level only the relevant information gets exposed; NOT CLEAR. Maybe more robust to include all the information.
-- NOTE the actionSpace will condition on  _slotStatus_ and on _missedBlock_ 
reportNonRegisteredProposer name actionSpace = [opengame|

    inputs    :  slotStatus, missedBlock, demand ;
    feedback  :   ;

    :---------------------------:
    inputs    :  slotStatus, missedBlock, demand ;
    feedback  :   ;
    operation :  dependentDecision name actionSpace ;
    outputs   :  reportNotRegistered ;
    returns   :  0 ;
    // Turn to payoffs later
    :---------------------------:

    outputs   :  reportNotRegistered ;
    returns   :   ;
  |]


-- Report missing request by proposer if slot was not missed and there was demand
-- NOTE the actionSpace will condition on  _slotStatus_, on _missedBlock_, and on _reportNotRegistered_
reportMissingRequest name actionSpace = [opengame|

    inputs    :  slotStatus, missedBlock, demand, reportNotRegistered;
    feedback  :   ;

    :---------------------------:
    inputs    :  slotStatus, slotStatus, missedBlock, demand, reportNotRegistered  ;
    feedback  :   ;
    operation :  dependentDecision name actionSpace ;
    outputs   :  reportMissingRequest ;
    returns   :  0 ;
    // Turn to payoffs later
    :---------------------------:

    outputs   :  reportMissingRequest ;
    returns   :   ;
  |]

-- Report missing reply by proposer if slot was not missed, there was demand, and proposer requested
-- NOTE the actionSpace will condition on  _slotStatus_, on _missedBlock_, on _reportNotRegistered_, and on _reportMissingRequest_
reportMissingReply name actionSpace = [opengame|

    inputs    :  slotStatus, missedBlock, demand, reportNotRegistered, reportMissingRequest;
    feedback  :   ;

    :---------------------------:
    inputs    :  slotStatus, missedBlock, demand, reportNotRegistered, reportMissingRequest ;
    feedback  :   ;
    operation :  dependentDecision name actionSpace ;
    outputs   :  reportMissingReply ;
    returns   :  0 ;
    // Turn to payoffs later
    :---------------------------:

    outputs   :  reportMissingReply ;
    returns   :   ;
  |]

-- Report reply after time-out  by proposer if slot was not missed, there was demand, proposer requested, and proposer replied
-- NOTE the actionSpace will condition on  _slotStatus_, on _missedBlock_, on _reportNotRegistered_, on _reportMissingRequest_, and on _reportMissingReply_
reportReplyTimeout name actionSpace = [opengame|

    inputs    :  slotStatus, missedBlock, demand, reportNotRegistered, reportMissingRequest, reportMissingReply;
    feedback  :   ;

    :---------------------------:
    inputs    :  slotStatus, missedBlock, demand, reportNotRegistered, reportMissingRequest, reportMissingReply ;
    feedback  :   ;
    operation :  dependentDecision name actionSpace ;
    outputs   :  reportReplyTimeout ;
    returns   :  0 ;
    // Turn to payoffs later
    :---------------------------:

    outputs   :  reportReplyTimeout ;
    returns   :   ;
  |]

-- Report wrong signature by proposer if slot was not missed, there was demand, proposer requested,  proposer replied, and did so before time-out
-- NOTE the actionSpace will condition on  _slotStatus_, on _missedBlock_, on _reportNotRegistered_, on _reportMissingRequest_, on _reportMissingReply_, and on _reportReplyTimeout_
reportWrongSignature name actionSpace = [opengame|

    inputs    :  slotStatus, missedBlock, demand, reportNotRegistered, reportMissingRequest, reportMissingReply, reportReplyTimeout;
    feedback  :   ;

    :---------------------------:
    inputs    :  slotStatus, missedBlock, demand, reportNotRegistered, reportMissingRequest, reportMissingReply, reportReplyTimeout ;
    feedback  :   ;
    operation :  dependentDecision name actionSpace ;
    outputs   :  reportWrongSignature ;
    returns   :  0 ;
    // Turn to payoffs later
    :---------------------------:

    outputs   :  reportWrongSignature ;
    returns   :   ;
  |]

-- Report missed slot due to builder if slot was not missed, there was demand, proposer requested,  proposer replied, and did so before time-out
-- NOTE the actionSpace will condition on  _slotStatus_, on _missedBlock_, on _reportNotRegistered_, on _reportMissingRequest_, on _reportMissingReply_, and on _reportReplyTimeout_
reportMissedSlot name actionSpace = [opengame|

    inputs    :  slotStatus, missedBlock, demand, reportNotRegistered, reportMissingRequest, reportMissingReply, reportReplyTimeout, reportWrongSignature;
    feedback  :   ;

    :---------------------------:
    inputs    :  slotStatus, missedBlock, demand, reportNotRegistered, reportMissingRequest, reportMissingReply, reportReplyTimeout, reportWrongSignature ;
    feedback  :   ;
    operation :  dependentDecision name actionSpace ;
    outputs   :  reportMissedSlot ;
    returns   :  0 ;
    // Turn to payoffs later
    :---------------------------:

    outputs   :  reportMissedSlot ;
    returns   :   ;
  |]



-- Report low payment by builder if slot was not missed, there was demand, proposer requested,  proposer replied,  did so before time-out, with the correct signature, and slot was not missed
-- NOTE the actionSpace will condition on  _slotStatus_, on _missedBlock_, on _reportNotRegistered_, on _reportMissingRequest_, on _reportMissingReply_, on _reportReplyTimeout_, on _reportWrongSignature_, and on _reportMissedSlot_
reportLowPayment name actionSpace = [opengame|

    inputs    :  slotStatus, missedBlock, demand, reportNotRegistered, reportMissingRequest, reportMissingReply, reportReplyTimeout, reportWrongSignature, reportMissedSlot;
    feedback  :   ;

    :---------------------------:
    inputs    :  slotStatus, missedBlock, demand, reportNotRegistered, reportMissingRequest, reportMissingReply, reportReplyTimeout, reportWrongSignature, reportMissedSlot ;
    feedback  :   ;
    operation :  dependentDecision name actionSpace ;
    outputs   :  reportLowPayment ;
    returns   :  0 ;
    // Turn to payoffs later
    :---------------------------:

    outputs   :  reportLowPayment ;
    returns   :   ;
  |]

-- Report proposer for violating status 
reportProposerFaultAndKicking name actionSpace = [opengame|

    inputs    :  slotStatus;
    feedback  :   ;

    :---------------------------:
    inputs    :  slotStatus;
    feedback  :   ;
    operation :  dependentDecision name actionSpace ;
    outputs   :  reportProposerViolatedStatus ;
    returns   :  0 ;
    // Turn to payoffs later
    :---------------------------:

    outputs   :  reportProposerViolatedStatus ;
    returns   :   ;
  |]



-- Aggregate reports
aggregateReports name actionSpaceNonRegisteredProposer actionSpaceMissingRequest actionSpaceMissingReply actionSpaceReplyTimeout actionSpaceMissedSlot actionSpaceLowPayment actionSpaceFaultAndKicking aggregateReportFunction actionSpaceWrongSignature payoffFunction = [opengame|

    inputs    :  slotStatus ;
    feedback  :   ;

    :---------------------------:

    inputs    :  slotStatus  ;
    feedback  :  ;
    operation :  reportMissingBlock name ;
    outputs   :  missedBlock,demand ;
    returns   :  ;


    inputs    :  slotStatus,missedBlock,demand ;
    feedback  :  ;
    operation :  reportNonRegisteredProposer name actionSpaceNonRegisteredProposer ;
    outputs   :  reportNotRegistered ;
    returns   :  ;

    inputs    :  slotStatus,missedBlock,demand, reportNotRegistered ;
    feedback  :  ;
    operation :  reportMissingRequest name actionSpaceMissingRequest ;
    outputs   :  reportMissingRequest ;
    returns   :  ;

    inputs    :  slotStatus,missedBlock,demand, reportNotRegistered, reportMissingRequest ;
    feedback  :  ;
    operation :  reportMissingReply name actionSpaceMissingReply ;
    outputs   :  reportMissingReply ;
    returns   :  ;

    inputs    :  slotStatus,missedBlock,demand, reportNotRegistered, reportMissingRequest, reportMissingReply ;
    feedback  :  ;
    operation :  reportReplyTimeout name actionSpaceReplyTimeout ;
    outputs   :  reportReplyTimeout ;
    returns   :  ;

    inputs    :  slotStatus,missedBlock,demand, reportNotRegistered, reportMissingRequest, reportMissingReply, reportReplyTimeout ;
    feedback  :  ;
    operation :  reportWrongSignature name actionSpaceWrongSignature ;
    outputs   :  reportWrongSignature ;
    returns   :  ;

    inputs    :  slotStatus,missedBlock,demand, reportNotRegistered, reportMissingRequest, reportMissingReply, reportReplyTimeout, reportWrongSignature ;
    feedback  :  ;
    operation :  reportMissedSlot name actionSpaceMissedSlot ;
    outputs   :  reportMissedSlot ;
    returns   :  ;

    inputs    :  slotStatus,missedBlock,demand, reportNotRegistered, reportMissingRequest, reportMissingReply, reportReplyTimeout, reportWrongSignature, reportMissedSlot ;
    feedback  :  ;
    operation :  reportLowPayment name actionSpaceLowPayment ;
    outputs   :  reportLowPayment ;
    returns   :  ;

    inputs    :  slotStatus;
    feedback  :  ;
    operation :  reportProposerFaultAndKicking name actionSpaceFaultAndKicking ;
    outputs   :  reportProposerFaultAndKicking ;
    returns   :  ;


    inputs    :  missedBlock,demand, reportNotRegistered, reportMissingRequest, reportMissingReply, reportReplyTimeout, reportWrongSignature, reportMissedSlot,  reportProposerFaultAndKicking;
    feedback  :  ;
    operation :  forwardFunction aggregateReportFunction ;
    outputs   :  report ;
    returns   :  ;

    inputs    :  report ;
    feedback  :  ;
    operation :  forwardFunction payoffFunction ;
    outputs   :  payoffReporter ;
    returns   :   ;

    inputs    :  payoffReporter ;
    feedback  :  ;
    operation :  addPayoffs name ;
    outputs   :  ;
    returns   :  ;



    :---------------------------:

    outputs   :   report ;
    returns   :   ;
  |]




