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
import SupportFunctions
import Components
import Types

import OpenGames.Engine.Engine
import OpenGames.Preprocessor


{-
Contains the full reporting model
-}


-- Assemble reporter components
report name payoutPool actionsGrievingProposer actionsMissingRequestProposer actionsMissingReplyProposer actionsReplyTimeout actionsWrongSignature actionsMissingRequestBuilder actionsMissingReplyBuilder actionsLowPayment actionsFaultAndKicking aggregateReportFunction penaltyValidator penaltyBuilder penaltyValidatorKicking verifyReportFunction paymentFunctionReporter payoffReporterParameters = [opengame|

    inputs    :  state, slotId, addrProposer, addrBuilder ;
    feedback  :   ;

    :---------------------------:

    inputs    :  state, slotId, addrProposer, addrBuilder;
    feedback  :   ;
    operation :  reporterDraft name ;
    outputs   :  internalReport, kickingReport ;
    returns   :  payments ;

    inputs    :  internalReport, kickingReport, addrProposer, addrBuilder ;
    feedback  :   ;
    operation :  submitReport name actionsOnChainReport penaltyValidator penaltyBuilder penaltyValidatorKicking;
    outputs   :  submittedReport ;
    returns   :  payments ;

    inputs    :  state, slotId, addrProposer, addrBuilder, submittedReport ;
    feedback  :   ;
    operation :  paymentsReporter name verifyReportFunction payoutPool paymentFunctionReporter payoffReporterParameters ;
    outputs   :  payments ;
    returns   :   ;


    :---------------------------:

    outputs   :  payments ;
    returns   :   ;
  |]
  where
     reporterDraft name = aggregateReportsPoNOffChain name payoutPool actionsGrievingProposer actionsMissingRequestProposer actionsMissingReplyProposer  actionsReplyTimeout actionsWrongSignature actionsMissingRequestBuilder actionsMissingReplyBuilder actionsLowPayment actionsFaultAndKicking aggregateReportFunction 
