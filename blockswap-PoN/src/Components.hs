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

import OpenGames.Engine.Engine
import OpenGames.Preprocessor

{-
We build a reporter as a composite of different games.
This should make it easier to extend and change things.
In principle, this also allows for multiple reporter "types"

NOTE: We paramterize by the action types. This allows to refine the reporting logic independently later. Here, we focus on the information flow. I.e. what has precendence. In order to keep the refinement of event reporting flexible, we also include full information on slots and preceding reporting conditions. Of course, this could be reduced but for now we want to keep the option open. 
-}

-------------------
-- 1 Reporter roles
-------------------

-- Is the proposer registered? Was the payment missed? And was there demand?
-- NOTE: We make these points as being detected and transformed as a function.
checkPreconditions name  = [opengame|

    inputs    :  slotStatus ;
    feedback  :   ;

    :---------------------------:
    inputs    :  slotStatus  ;
    feedback  :   ;
    operation :  forwardFunction checkRegistered;
    outputs   :  registeredProposer ;
    returns   :   ;

    inputs    :  slotStatus  ;
    feedback  :   ;
    operation :  forwardFunction checkMissedPayment;
    outputs   :  missedPayment ;
    returns   :   ;

    inputs    :  slotStatus ;
    feedback  :   ;
    operation :  forwardFunction checkDemand;
    outputs   :  demand ;
    returns   :  ;

    :---------------------------:

    outputs   :  registeredProposer, missedPayment, demand ;
    returns   :   ;
  |]

-- Report proposer if registered proposer, demand was present but no fee received in the payout pool
-- NOTE the actionSpace will condition on _slotStatus_, on _missedBlock_, and on _demand_
reportGrievingProposer name actionSpace = [opengame|

    inputs    :  slotStatus, registeredProposer, missedPayment, demand ;
    feedback  :   ;

    :---------------------------:
    inputs    :  slotStatus,registeredProposer, missedPayment, demand ;
    feedback  :   ;
    operation :  dependentDecision name actionSpace ;
    outputs   :  reportGrieving ;
    returns   :  0 ;
    // Turn to payoffs later
    :---------------------------:

    outputs   :  reportGrieving ;
    returns   :   ;
  |]


-- Report missing request by proposer if payment was not missed and there was demand
-- NOTE the actionSpace will condition on  _slotStatus,registeredProposer_, on _missedBlock_, on _demand_, and on _reportNotRegistered_
reportMissingRequestProposer name actionSpace = [opengame|

    inputs    :  slotStatus, registeredProposer, missedPayment, demand, reportGrieving;
    feedback  :   ;

    :---------------------------:
    inputs    :  slotStatus,registeredProposer, missedPayment, demand, reportGrieving  ;
    feedback  :   ;
    operation :  dependentDecision name actionSpace ;
    outputs   :  reportMissingRequestProposer ;
    returns   :  0 ;
    // Turn to payoffs later
    :---------------------------:

    outputs   :  reportMissingRequestProposer ;
    returns   :   ;
  |]

-- Report missing reply by proposer if payment was not missed, there was demand, and proposer requested
-- NOTE the actionSpace will condition on  _slotStatus,registeredProposer_, on _missedBlock_, on _demand_, on _reportNotRegistered_, and on _reportMissingRequest_
reportMissingReplyProposer name actionSpace = [opengame|

    inputs    :  slotStatus, registeredProposer, missedPayment, demand, reportGrieving, reportMissingRequestProposer;
    feedback  :   ;

    :---------------------------:
    inputs    :  slotStatus,registeredProposer, missedPayment, demand, reportGrieving, reportMissingRequestProposer ;
    feedback  :   ;
    operation :  dependentDecision name actionSpace ;
    outputs   :  reportMissingReplyProposer ;
    returns   :  0 ;
    // Turn to payoffs later
    :---------------------------:

    outputs   :  reportMissingReplyProposer ;
    returns   :   ;
  |]

-- Report reply after time-out  by proposer if payment was not missed, there was demand, proposer requested, and proposer replied
-- NOTE the actionSpace will condition on  _slotStatus,registeredProposer_, on _missedBlock_, on _demand_ on _reportNotRegistered_, on _reportMissingRequest_, and on _reportMissingReply_
reportReplyTimeout name actionSpace = [opengame|

    inputs    :  slotStatus,registeredProposer, missedPayment, demand, reportGrieving, reportMissingRequestProposer, reportMissingReplyProposer;
    feedback  :   ;

    :---------------------------:
    inputs    :  slotStatus,registeredProposer, missedPayment, demand, reportGrieving, reportMissingRequestProposer, reportMissingReplyProposer ;
    feedback  :   ;
    operation :  dependentDecision name actionSpace ;
    outputs   :  reportReplyTimeout ;
    returns   :  0 ;
    // Turn to payoffs later
    :---------------------------:

    outputs   :  reportReplyTimeout ;
    returns   :   ;
  |]

-- Report wrong signature by proposer if payment was not missed, there was demand, proposer requested,  proposer replied, and did so before time-out
-- NOTE the actionSpace will condition on  _slotStatus,registeredProposer_, on _missedBlock_, on _demand_ on _reportNotRegistered_, on _reportMissingRequest_, on _reportMissingReply_, and on _reportReplyTimeout_
reportSignature name actionSpace = [opengame|

    inputs    :  slotStatus,registeredProposer, missedPayment, demand, reportGrieving, reportMissingRequestProposer, reportMissingReplyProposer, reportReplyTimeout;
    feedback  :   ;

    :---------------------------:
    inputs    :  slotStatus,registeredProposer, missedPayment, demand, reportGrieving, reportMissingRequestProposer, reportMissingReplyProposer, reportReplyTimeout ;
    feedback  :   ;
    operation :  dependentDecision name actionSpace ;
    outputs   :  reportSignature ;
    returns   :  0 ;
    // Turn to payoffs later
    :---------------------------:

    outputs   :  reportSignature ;
    returns   :   ;
  |]

-- Report missed slot due to builder because of not sending a request. Assumes that payment was not missed, there was demand, proposer requested,  proposer replied, did so before time-out, and signed it correctly
-- NOTE the actionSpace will condition on  _slotStatus,registeredProposer_, on _missedBlock_, on _demand_ on _reportNotRegistered_, on _reportMissingRequest_, on _reportMissingReply_, on _reportReplyTimeout_, and on _reportSignature_
reportMissingRequestBuilder name actionSpace = [opengame|

    inputs    :  slotStatus,registeredProposer, missedPayment, demand, reportGrieving, reportMissingRequestProposer, reportMissingReplyProposer, reportReplyTimeout, reportSignature;
    feedback  :   ;

    :---------------------------:
    inputs    :  slotStatus,registeredProposer, missedPayment, demand, reportGrieving, reportMissingRequestProposer, reportMissingReplyProposer, reportReplyTimeout, reportSignature ;
    feedback  :   ;
    operation :  dependentDecision name actionSpace ;
    outputs   :  reportMissingRequestBuilder ;
    returns   :  0 ;
    // Turn to payoffs later
    :---------------------------:

    outputs   :  reportMissingRequestBuilder ;
    returns   :   ;
  |]

-- Report missed slot due to builder because of not proposing a block. Assumes that  payment was not missed, there was demand, proposer requested,  proposer replied, did so before time-out, and signed it correctly
-- NOTE the actionSpace will condition on  _slotStatus,registeredProposer_, on _missedBlock_, on _demand_ on _reportNotRegistered_, on _reportMissingRequest_, on _reportMissingReply_, on _reportReplyTimeout_, on _reportSignature_, and on _reportMissingRequestBuilder_
reportMissingReplyBuilder name actionSpace = [opengame|

    inputs    :  slotStatus,registeredProposer, missedPayment, demand, reportGrieving, reportMissingRequestProposer, reportMissingReplyProposer, reportReplyTimeout, reportSignature, reportMissingRequestBuilder;
    feedback  :   ;

    :---------------------------:
    inputs    :  slotStatus,registeredProposer, missedPayment, demand, reportGrieving, reportMissingRequestProposer, reportMissingReplyProposer, reportReplyTimeout, reportSignature, reportMissingRequestBuilder ;
    feedback  :   ;
    operation :  dependentDecision name actionSpace ;
    outputs   :  reportMissingReplyBuilder ;
    returns   :  0 ;
    // Turn to payoffs later
    :---------------------------:

    outputs   :  reportMissingReplyBuilder ;
    returns   :   ;
  |]



-- Report low payment by builder if payment was not missed, there was demand, proposer requested,  proposer replied,  did so before time-out, with the correct signature, and slot was not missed
-- NOTE the actionSpace will condition on  _slotStatus,registeredProposer_, on _missedBlock_, on _demand_ on _reportNotRegistered_, on _reportMissingRequest_, on _reportMissingReply_, on _reportReplyTimeout_, on _reportWrongSignature_, and on _reportMissedSlot_
reportLowPayment name actionSpace = [opengame|

    inputs    :  slotStatus,registeredProposer, missedPayment, demand, reportGrieving, reportMissingRequestProposer, reportMissingReplyProposer, reportReplyTimeout, reportSignature, reportMissingRequestBuilder, reportMissingReplyBuilder;
    feedback  :   ;

    :---------------------------:
    inputs    :  slotStatus,registeredProposer, missedPayment, demand, reportGrieving, reportMissingRequestProposer, reportMissingReplyProposer, reportReplyTimeout, reportSignature, reportMissingRequestBuilder, reportMissingReplyBuilder ;
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

    inputs    :  slotStatus,registeredProposer;
    feedback  :   ;

    :---------------------------:
    inputs    :  slotStatus,registeredProposer;
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
aggregateReports name actionSpaceGrievingProposer actionSpaceMissingRequestProposer actionSpaceMissingReplyProposer  actionSpaceReplyTimeout actionSpaceWrongSignature actionSpaceMissingRequestBuilder actionSpaceMissingReplyBuilder actionSpaceLowPayment actionSpaceFaultAndKicking aggregateReportFunction payoffFunction = [opengame|

    inputs    :  slotStatus ;
    feedback  :   ;

    :---------------------------:

    inputs    :  slotStatus  ;
    feedback  :  ;
    operation :  checkPreconditions name ;
    outputs   :  registeredProposer, missedPayment, demand ;
    returns   :  ;


    inputs    :  slotStatus, registeredProposer, missedPayment, demand ;
    feedback  :  ;
    operation :  reportGrievingProposer name actionSpaceGrievingProposer ;
    outputs   :  reportGrieving ;
    returns   :  ;

    inputs    :  slotStatus, registeredProposer, missedPayment, demand, reportGrieving ;
    feedback  :  ;
    operation :  reportMissingRequestProposer name actionSpaceMissingRequestProposer ;
    outputs   :  reportMissingRequestProposer ;
    returns   :  ;

    inputs    :  slotStatus, registeredProposer, missedPayment, demand, reportGrieving, reportMissingRequestProposer ;
    feedback  :  ;
    operation :  reportMissingReplyProposer name actionSpaceMissingReplyProposer ;
    outputs   :  reportMissingReplyProposer ;
    returns   :  ;

    inputs    :  slotStatus,registeredProposer,missedPayment,demand, reportGrieving, reportMissingRequestProposer, reportMissingReplyProposer ;
    feedback  :  ;
    operation :  reportReplyTimeout name actionSpaceReplyTimeout ;
    outputs   :  reportReplyTimeout ;
    returns   :  ;

    inputs    :  slotStatus,registeredProposer,missedPayment,demand, reportGrieving, reportMissingRequestProposer, reportMissingReplyProposer, reportReplyTimeout ;
    feedback  :  ;
    operation :  reportSignature name actionSpaceWrongSignature ;
    outputs   :  reportSignature ;
    returns   :  ;

    inputs    :  slotStatus,registeredProposer,missedPayment,demand, reportGrieving, reportMissingRequestProposer, reportMissingReplyProposer, reportReplyTimeout, reportSignature ;
    feedback  :  ;
    operation :  reportMissingRequestBuilder name actionSpaceMissingRequestBuilder ;
    outputs   :  reportMissingRequestBuilder ;
    returns   :  ;

    inputs    :  slotStatus,registeredProposer,missedPayment,demand, reportGrieving, reportMissingRequestProposer, reportMissingReplyProposer, reportReplyTimeout, reportSignature, reportMissingRequestBuilder ;
    feedback  :  ;
    operation :  reportMissingReplyBuilder name actionSpaceMissingReplyBuilder ;
    outputs   :  reportMissingReplyBuilder ;
    returns   :  ;

    inputs    :  slotStatus,registeredProposer,missedPayment,demand, reportGrieving, reportMissingRequestProposer, reportMissingReplyProposer, reportReplyTimeout, reportSignature, reportMissingRequestBuilder, reportMissingReplyBuilder;
    feedback  :  ;
    operation :  reportLowPayment name actionSpaceLowPayment ;
    outputs   :  reportLowPayment ;
    returns   :  ;

    inputs    :  slotStatus,registeredProposer;
    feedback  :  ;
    operation :  reportProposerFaultAndKicking name actionSpaceFaultAndKicking ;
    outputs   :  reportProposerFaultAndKicking ;
    returns   :  ;

    inputs    :  missedPayment,demand, reportGrieving, reportMissingRequestProposer, reportMissingReplyProposer, reportReplyTimeout, reportSignature, reportMissingRequestBuilder, reportMissingReplyBuilder, reportLowPayment, reportProposerFaultAndKicking;
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


