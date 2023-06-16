{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}


module Model
  where

import ActionSpaces
import Components
import PayoutPoolFunctionality
import SupportFunctions
import Types

import OpenGames.Engine.Engine
import OpenGames.Preprocessor


{-
Contains the full reporting model
-}


-- Assemble reporter components
report name actionsGrievingProposer actionsMissingRequestProposer actionsMissingReplyProposer actionsReplyTimeout actionsWrongSignature actionsMissingRequestBuilder actionsMissingReplyBuilder actionsLowPayment actionsFaultAndKicking aggregateReportFunction penaltyValidator penaltyBuilder penaltyValidatorKicking verifyReportFunction paymentFunctionReporter payoffReporterParameters submissionCosts = [opengame|

    inputs    :  state, slotId, addrProposer, addrBuilder ;
    feedback  :   ;

    :---------------------------:

    inputs    :  state, slotId, addrProposer, addrBuilder;
    feedback  :   ;
    operation :  reporterDraft name ;
    outputs   :  internalReport, kickingReport ;
    returns   :  payments ;

    inputs    :  state, slotId, addrProposer, addrBuilder, internalReport, kickingReport ;
    feedback  :   ;
    operation :  submitReport name actionsOnChainReport penaltyValidator penaltyBuilder penaltyValidatorKicking submissionCosts ;
    outputs   :  submittedReport ;
    returns   :  payments ;

    inputs    :  state, slotId, addrProposer, addrBuilder, submittedReport ;
    feedback  :   ;
    operation :  paymentsReporter name verifyReportFunction paymentFunctionReporter payoffReporterParameters ;
    outputs   :  payments ;
    returns   :   ;


    :---------------------------:

    outputs   :  payments ;
    returns   :   ;
  |]
  where
     reporterDraft name = aggregateReportsPoNOffChain name actionsGrievingProposer actionsMissingRequestProposer actionsMissingReplyProposer  actionsReplyTimeout actionsWrongSignature actionsMissingRequestBuilder actionsMissingReplyBuilder actionsLowPayment actionsFaultAndKicking aggregateReportFunction 


-- Assemble reporter components
-- Connected to payoutPool update
reportWPayoutPool name actionsGrievingProposer actionsMissingRequestProposer actionsMissingReplyProposer actionsReplyTimeout actionsWrongSignature actionsMissingRequestBuilder actionsMissingReplyBuilder actionsLowPayment actionsFaultAndKicking aggregateReportFunction penaltyValidator penaltyBuilder penaltyValidatorKicking verifyReportFunction paymentFunctionReporter payoffReporterParameters reporterAddr submissionCosts = [opengame|

    inputs    :  state, slotId, addrProposer, addrBuilder ;
    feedback  :   ;

    :---------------------------:

    inputs    :  state, slotId, addrProposer, addrBuilder;
    feedback  :   ;
    operation :  reporterDraft name ;
    outputs   :  internalReport, kickingReport ;
    returns   :  payments ;

    inputs    :  state, slotId, addrProposer, addrBuilder, internalReport, kickingReport ;
    feedback  :   ;
    operation :  submitReport name actionsOnChainReport penaltyValidator penaltyBuilder penaltyValidatorKicking submissionCosts;
    outputs   :  submittedReport ;
    returns   :  payments ;

    inputs    :  state, slotId, addrProposer, addrBuilder, submittedReport ;
    feedback  :   ;
    operation :  paymentsReporterForwardReport name verifyReportFunction paymentFunctionReporter payoffReporterParameters forwardReport ;
    outputs   :  payments, reportForwarded ;
    returns   :   ;

    inputs    :  state, reportForwarded ;
    feedback  :   ;
    operation :  updatePayoutPool reporterAddr reporterToPayoutPool;
    outputs   :  stateNew ;
    returns   :   ;


     :---------------------------:

    outputs   :  payments, stateNew ;
    returns   :   ;
  |]
  where
     reporterDraft name = aggregateReportsPoNOffChain name actionsGrievingProposer actionsMissingRequestProposer actionsMissingReplyProposer  actionsReplyTimeout actionsWrongSignature actionsMissingRequestBuilder actionsMissingReplyBuilder actionsLowPayment actionsFaultAndKicking aggregateReportFunction 


