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
report name  actionsGrievingProposer actionsMissingRequestProposer actionsMissingReplyProposer actionsReplyTimeout actionsWrongSignature actionsMissingRequestBuilder actionsMissingReplyBuilder actionsLowPayment actionsFaultAndKicking aggregateReportFunction verifyReportFunction paymentFunctionReporter = [opengame|

    inputs    :  slotStatus ;
    feedback  :   ;

    :---------------------------:

    inputs    :  slotStatus ;
    feedback  :   ;
    operation :  reporterDraft name ;
    outputs   :  internalReport ;
    returns   :  payments ;

    inputs    :  internalReport ;
    feedback  :   ;
    operation :  submitReport name actionsOnChainReport ;
    outputs   :  submittedReport ;
    returns   :  payments ;

    inputs    :  submittedReport, slotStatus ;
    feedback  :   ;
    operation :  paymentsReporter name verifyReportFunction paymentFunctionReporter ;
    outputs   :  payments ;
    returns   :   ;


    :---------------------------:

    outputs   :  payments ;
    returns   :   ;
  |]
  where
     reporterDraft name  = aggregateReportsPoNOffChain name actionsGrievingProposer actionsMissingRequestProposer actionsMissingReplyProposer  actionsReplyTimeout actionsWrongSignature actionsMissingRequestBuilder actionsMissingReplyBuilder actionsLowPayment actionsFaultAndKicking aggregateReportFunction 
