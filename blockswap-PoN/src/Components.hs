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

import Data.Tuple.Extra (uncurry3)

{-
We build a reporter as a composite of different games.
This should make it easier to extend and change things.
In principle, this also allows for multiple reporter "types"

NOTE: We paramterize by the action types. This allows to refine the reporting logic independently later. Here, we focus on the information flow. I.e. what has precendence. In order to keep the refinement of event reporting flexible, we also include full information on slots and preceding reporting conditions. Of course, this could be reduced but for now we want to keep the option open. 
-}

----------------------------------------
-- 1 Reporter roles internal (off-chain)
----------------------------------------

-- We have access to the current state, the addr of the proposer, and the address of the builder, and the slotId of a potential report-worthy event 
-- Is the proposer registered? Was the payment missed? And was there demand?
-- NOTE: We make these points as being detected and transformed as a function.
checkPreconditions name payoutPool = [opengame|

    inputs    :  state, slotId, addrProposer, addrBuilder ;
    feedback  :   ;

    :---------------------------:
    inputs    :  state, slotId  ;
    feedback  :   ;
    operation :  forwardFunction $ uncurry $ checkReportInterval payoutPool;
    outputs   :  slotInPast ;
    returns   :   ;

    inputs    :  state, addrProposer  ;
    feedback  :   ;
    operation :  forwardFunction $ uncurry checkRegistered;
    outputs   :  registeredProposer ;
    returns   :   ;

    inputs    :  state, slotId, addrBuilder  ;
    feedback  :   ;
    operation :  forwardFunction $ uncurry3 checkPayment;
    outputs   :  missedPayment ;
    returns   :   ;

    inputs    :  state, slotId ;
    feedback  :   ;
    operation :  forwardFunction $ uncurry checkDemand;
    outputs   :  demand ;
    returns   :  ;

    :---------------------------:

    outputs   :  slotInPast, registeredProposer, missedPayment, demand ;
    returns   :   ;
  |]


  
-- Report proposer if registered proposer, demand was present but no fee received in the payout pool
-- NOTE the actionSpace will condition on _slotStatus_, on _missedBlock_, and on _demand_
reportGrievingProposer name actionSpace = [opengame|

    inputs    :  state, slotId, addrProposer, addrBuilder, slotInPast, registeredProposer, missedPayment, demand ;
    feedback  :   ;

    :---------------------------:

    inputs    :  state, slotId, addrProposer, addrBuilder, slotInPast, registeredProposer, missedPayment, demand ;
    feedback  :   ;
    operation :  dependentDecision name actionSpace ;
    outputs   :  reportGrieving ;
    returns   :  payments ;

    :---------------------------:

    outputs   :  reportGrieving ;
    returns   :  payments ;
  |]


-- Report missing request by proposer if payment was not missed and there was demand
-- NOTE the actionSpace will condition on  _slotStatus,registeredProposer_, on _missedBlock_, on _demand_, and on _reportNotRegistered_
reportMissingRequestProposer name actionSpace = [opengame|

    inputs    :  state, slotId, addrProposer, addrBuilder, slotInPast, registeredProposer, missedPayment, demand, reportGrieving;
    feedback  :   ;

    :---------------------------:

    inputs    :  state, slotId, addrProposer, addrBuilder, slotInPast, registeredProposer, missedPayment, demand, reportGrieving  ;
    feedback  :   ;
    operation :  dependentDecision name actionSpace ;
    outputs   :  reportMissingRequestProposer ;
    returns   :  payments ;

    :---------------------------:

    outputs   :  reportMissingRequestProposer ;
    returns   :  payments ;
  |]

-- Report missing reply by proposer if payment was not missed, there was demand, and proposer requested
-- NOTE the actionSpace will condition on  _slotStatus,registeredProposer_, on _missedBlock_, on _demand_, on _reportNotRegistered_, and on _reportMissingRequest_
reportMissingReplyProposer name actionSpace = [opengame|

    inputs    :  state, slotId, addrProposer, addrBuilder, slotInPast, registeredProposer, missedPayment, demand, reportGrieving, reportMissingRequestProposer;
    feedback  :   ;

    :---------------------------:

    inputs    :  state, slotId, addrProposer, addrBuilder, slotInPast, registeredProposer, missedPayment, demand, reportGrieving, reportMissingRequestProposer ;
    feedback  :   ;
    operation :  dependentDecision name actionSpace ;
    outputs   :  reportMissingReplyProposer ;
    returns   :  payments ;

    :---------------------------:

    outputs   :  reportMissingReplyProposer ;
    returns   :  payments ;
  |]

-- Report reply after time-out  by proposer if payment was not missed, there was demand, proposer requested, and proposer replied
-- NOTE the actionSpace will condition on  _slotStatus,registeredProposer_, on _missedBlock_, on _demand_ on _reportNotRegistered_, on _reportMissingRequest_, and on _reportMissingReply_
reportReplyTimeout name actionSpace = [opengame|

    inputs    :  state, slotId, addrProposer, addrBuilder, slotInPast, registeredProposer, missedPayment, demand, reportGrieving, reportMissingRequestProposer, reportMissingReplyProposer;
    feedback  :   ;

    :---------------------------:

    inputs    :  state, slotId, addrProposer, addrBuilder, slotInPast, registeredProposer, missedPayment, demand, reportGrieving, reportMissingRequestProposer, reportMissingReplyProposer ;
    feedback  :   ;
    operation :  dependentDecision name actionSpace ;
    outputs   :  reportReplyTimeout ;
    returns   :  payments ;

    :---------------------------:

    outputs   :  reportReplyTimeout ;
    returns   :  payments ;
  |]

-- Report wrong signature by proposer if payment was not missed, there was demand, proposer requested,  proposer replied, and did so before time-out
-- NOTE the actionSpace will condition on  _slotStatus,registeredProposer_, on _missedBlock_, on _demand_ on _reportNotRegistered_, on _reportMissingRequest_, on _reportMissingReply_, and on _reportReplyTimeout_
reportSignature name actionSpace = [opengame|

    inputs    :  state, slotId, addrProposer, addrBuilder, slotInPast, registeredProposer, missedPayment, demand, reportGrieving, reportMissingRequestProposer, reportMissingReplyProposer, reportReplyTimeout;
    feedback  :   ;

    :---------------------------:

    inputs    :  state, slotId, addrProposer, addrBuilder, slotInPast, registeredProposer, missedPayment, demand, reportGrieving, reportMissingRequestProposer, reportMissingReplyProposer, reportReplyTimeout ;
    feedback  :   ;
    operation :  dependentDecision name actionSpace ;
    outputs   :  reportSignature ;
    returns   :  payments ;

    :---------------------------:

    outputs   :  reportSignature ;
    returns   :  payments ;
  |]

-- Report missed slot due to builder because of not sending a request. Assumes that payment was not missed, there was demand, proposer requested,  proposer replied, did so before time-out, and signed it correctly
-- NOTE the actionSpace will condition on  _slotStatus,registeredProposer_, on _missedBlock_, on _demand_ on _reportNotRegistered_, on _reportMissingRequest_, on _reportMissingReply_, on _reportReplyTimeout_, and on _reportSignature_
reportMissingRequestBuilder name actionSpace = [opengame|

    inputs    :  state, slotId, addrProposer, addrBuilder, slotInPast, registeredProposer, missedPayment, demand, reportGrieving, reportMissingRequestProposer, reportMissingReplyProposer, reportReplyTimeout, reportSignature;
    feedback  :   ;

    :---------------------------:

    inputs    :  state, slotId, addrProposer, addrBuilder, slotInPast, registeredProposer, missedPayment, demand, reportGrieving, reportMissingRequestProposer, reportMissingReplyProposer, reportReplyTimeout, reportSignature ;
    feedback  :   ;
    operation :  dependentDecision name actionSpace ;
    outputs   :  reportMissingRequestBuilder ;
    returns   :  payments ;

    :---------------------------:

    outputs   :  reportMissingRequestBuilder ;
    returns   :  payments ;
  |]

-- Report missed slot due to builder because of not proposing a block. Assumes that  payment was not missed, there was demand, proposer requested,  proposer replied, did so before time-out, and signed it correctly
-- NOTE the actionSpace will condition on  _slotStatus,registeredProposer_, on _missedBlock_, on _demand_ on _reportNotRegistered_, on _reportMissingRequest_, on _reportMissingReply_, on _reportReplyTimeout_, on _reportSignature_, and on _reportMissingRequestBuilder_
reportMissingReplyBuilder name actionSpace = [opengame|

    inputs    :  state, slotId, addrProposer, addrBuilder, slotInPast, registeredProposer, missedPayment, demand, reportGrieving, reportMissingRequestProposer, reportMissingReplyProposer, reportReplyTimeout, reportSignature, reportMissingRequestBuilder;
    feedback  :   ;

    :---------------------------:

    inputs    :  state, slotId, addrProposer, addrBuilder, slotInPast, registeredProposer, missedPayment, demand, reportGrieving, reportMissingRequestProposer, reportMissingReplyProposer, reportReplyTimeout, reportSignature, reportMissingRequestBuilder ;
    feedback  :   ;
    operation :  dependentDecision name actionSpace ;
    outputs   :  reportMissingReplyBuilder ;
    returns   :  payments ;

    :---------------------------:

    outputs   :  reportMissingReplyBuilder ;
    returns   :  payments ;
  |]



-- Report low payment by builder if payment was not missed, there was demand, proposer requested,  proposer replied,  did so before time-out, with the correct signature, and slot was not missed
-- NOTE the actionSpace will condition on  _slotStatus,registeredProposer_, on _missedBlock_, on _demand_ on _reportNotRegistered_, on _reportMissingRequest_, on _reportMissingReply_, on _reportReplyTimeout_, on _reportWrongSignature_, and on _reportMissedSlot_
reportLowPayment name actionSpace = [opengame|

    inputs    :  state, slotId, addrProposer, addrBuilder, slotInPast, registeredProposer, missedPayment, demand, reportGrieving, reportMissingRequestProposer, reportMissingReplyProposer, reportReplyTimeout, reportSignature, reportMissingRequestBuilder, reportMissingReplyBuilder;
    feedback  :   ;

    :---------------------------:

    inputs    :  state, slotId, addrProposer, addrBuilder, slotInPast, registeredProposer, missedPayment, demand, reportGrieving, reportMissingRequestProposer, reportMissingReplyProposer, reportReplyTimeout, reportSignature, reportMissingRequestBuilder, reportMissingReplyBuilder ;
    feedback  :   ;
    operation :  dependentDecision name actionSpace ;
    outputs   :  reportLowPayment ;
    returns   :  payments ;

    :---------------------------:

    outputs   :  reportLowPayment ;
    returns   :  payments ;
  |]

-- Report proposer for violating status
reportProposerFaultAndKicking name actionSpace = [opengame|

    inputs    :  state, slotId, addrProposer, addrBuilder, registeredProposer;
    feedback  :   ;

    :---------------------------:

    inputs    :  state, slotId, addrProposer, addrBuilder, registeredProposer;
    feedback  :   ;
    operation :  dependentDecision name actionSpace ;
    outputs   :  reportProposerViolatedStatus ;
    returns   :  payments ;

    :---------------------------:

    outputs   :  reportProposerViolatedStatus ;
    returns   :  payments ;
  |]

-------------------------------
-- 2 Aggregate internal reports
-------------------------------

-- Aggregate reports from comnining on-chain and off-chain data
-- This structures the internal logic of the offchain component
-- NOTE: we assume that the reporter has access to the on-chain state as well as the off-chain states; in particular he can inspect the different relays and messages sent or not sent
-- NOTE: The current structure allows for an internally distributed way the reporting works. There could be even internal remuneration. Also note that we distinguish a further step where the external onchain report is filed
aggregateReportsPoNOffChain name payoutPool actionSpaceGrievingProposer actionSpaceMissingRequestProposer actionSpaceMissingReplyProposer  actionSpaceReplyTimeout actionSpaceWrongSignature actionSpaceMissingRequestBuilder actionSpaceMissingReplyBuilder actionSpaceLowPayment actionSpaceFaultAndKicking aggregateReportFunction = [opengame|

    inputs    :  state, slotId, addrProposer, addrBuilder  ;
    feedback  :   ;

    :---------------------------:

    inputs    :  state, slotId, addrProposer, addrBuilder  ;
    feedback  :  ;
    operation :  checkPreconditions name payoutPool ;
    outputs   :  slotInPast, registeredProposer, missedPayment, demand ;
    returns   :  ;


    inputs    :  state, slotId, addrProposer, addrBuilder, slotInPast, registeredProposer, missedPayment, demand ;
    feedback  :  ;
    operation :  reportGrievingProposer name actionSpaceGrievingProposer ;
    outputs   :  reportGrieving ;
    returns   :  payments ;

    inputs    :  state, slotId, addrProposer, addrBuilder, slotInPast, registeredProposer, missedPayment, demand, reportGrieving ;
    feedback  :  ;
    operation :  reportMissingRequestProposer name actionSpaceMissingRequestProposer ;
    outputs   :  reportMissingRequestProposer ;
    returns   :  payments ;

    inputs    :  state, slotId, addrProposer, addrBuilder, slotInPast, registeredProposer, missedPayment, demand, reportGrieving, reportMissingRequestProposer ;
    feedback  :  ;
    operation :  reportMissingReplyProposer name actionSpaceMissingReplyProposer ;
    outputs   :  reportMissingReplyProposer ;
    returns   :  payments ;

    inputs    :  state, slotId, addrProposer, addrBuilder, slotInPast, registeredProposer,missedPayment,demand, reportGrieving, reportMissingRequestProposer, reportMissingReplyProposer ;
    feedback  :  ;
    operation :  reportReplyTimeout name actionSpaceReplyTimeout ;
    outputs   :  reportReplyTimeout ;
    returns   :  payments ;

    inputs    :  state, slotId, addrProposer, addrBuilder, slotInPast, registeredProposer,missedPayment,demand, reportGrieving, reportMissingRequestProposer, reportMissingReplyProposer, reportReplyTimeout ;
    feedback  :  ;
    operation :  reportSignature name actionSpaceWrongSignature ;
    outputs   :  reportSignature ;
    returns   :  payments ;

    inputs    :  state, slotId, addrProposer, addrBuilder, slotInPast, registeredProposer,missedPayment,demand, reportGrieving, reportMissingRequestProposer, reportMissingReplyProposer, reportReplyTimeout, reportSignature ;
    feedback  :  ;
    operation :  reportMissingRequestBuilder name actionSpaceMissingRequestBuilder ;
    outputs   :  reportMissingRequestBuilder ;
    returns   :  payments ;

    inputs    :  state, slotId, addrProposer, addrBuilder, slotInPast, registeredProposer,missedPayment,demand, reportGrieving, reportMissingRequestProposer, reportMissingReplyProposer, reportReplyTimeout, reportSignature, reportMissingRequestBuilder ;
    feedback  :  ;
    operation :  reportMissingReplyBuilder name actionSpaceMissingReplyBuilder ;
    outputs   :  reportMissingReplyBuilder ;
    returns   :  payments ;

    inputs    :  state, slotId, addrProposer, addrBuilder, slotInPast, registeredProposer,missedPayment,demand, reportGrieving, reportMissingRequestProposer, reportMissingReplyProposer, reportReplyTimeout, reportSignature, reportMissingRequestBuilder, reportMissingReplyBuilder;
    feedback  :  ;
    operation :  reportLowPayment name actionSpaceLowPayment ;
    outputs   :  reportLowPayment ;
    returns   :  payments ;

    inputs    :  state, slotId, addrProposer, addrBuilder,registeredProposer;
    feedback  :  ;
    operation :  reportProposerFaultAndKicking name actionSpaceFaultAndKicking ;
    outputs   :  reportProposerFaultAndKicking ;
    returns   :  payments ;

    inputs    :  slotInPast, registeredProposer, missedPayment,demand, reportGrieving, reportMissingRequestProposer, reportMissingReplyProposer, reportReplyTimeout, reportSignature, reportMissingRequestBuilder, reportMissingReplyBuilder, reportLowPayment, reportProposerFaultAndKicking;
    feedback  :  ;
    operation :  forwardFunction aggregateReportFunction ;
    outputs   :  report ;
    returns   :   ;

    :---------------------------:

    outputs   :  report  ;
    returns   :  payments ;
  |]

----------------------
-- 3 Reporter on-chain
----------------------

-- On the basis of received information submit on-chain report
submitReport name actionSpace penaltyValidator penaltyBuilder penaltyValidatorKicking = [opengame|

    inputs    :  internalReport, addrProposer, addrBuilder ;
    feedback  :   ;

    :---------------------------:

    inputs    :  internalReport, addrProposer, addrBuilder ;
    feedback  :   ;
    operation :  dependentDecision name (actionSpace penaltyValidator penaltyBuilder penaltyValidatorKicking);
    outputs   :  submittedReport ;
    returns   :  payments ;

    :---------------------------:

    outputs   :  submittedReport ;
    returns   :  payments ;
  |]

------------------------
-- 4 Payoff verification
------------------------

-- On the basis of received on-chain report and access to the on-chain and off-chain states 
paymentsReporter name verifyReportFunction payoutPool paymentFunctionReporter payoffReporterParameters = [opengame|

    inputs    :  state, slotId, addrProposer, addrBuilder, submittedReport;
    feedback  :   ;

    :---------------------------:

    inputs    :  state, slotId, addrProposer, addrBuilder, submittedReport ;
    feedback  :   ;
    operation :  forwardFunction $ verifyReportFunction payoutPool ;
    outputs   :  reportVerified ;
    returns   :   ;

    inputs    :  reportVerified ;
    feedback  :   ;
    operation :  forwardFunction (paymentFunctionReporter payoffReporterParameters);
    outputs   :  payments ;
    returns   :   ;

    :---------------------------:

    outputs   :  payments ;
    returns   :   ;
  |]



