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
We build a reporter a composite of different games.
This should make it easier to extend and changes things.
In principle, this also allows for multiple reporter "types"
-}

-------------------
-- 1 Reporter roles
-------------------

-- Report registered proposer
-- Report proposer if registered proposer and payment received but no block requested
-- MAYBE: Filter through the information; so that at each level only the relevant information gets exposed; NOT CLEAR. Maybe more robust to include all the information.
reportRegisteredProposer name actionSpace = [opengame|

    inputs    :  slotStatus ;
    feedback  :   ;

    :---------------------------:
    inputs    :  slotStatus  ;
    feedback  :   ;
    operation :  dependentDecision name actionSpace ;
    outputs   :  reportRegister ;
    returns   :  0 ;
    // Turn to payoffs later
    :---------------------------:

    outputs   :  reportRegister ;
    returns   :   ;
  |]


-- Report missing request by proposer if slot was not missed and there was demand
reportMissingRequest name actionSpace = [opengame|

    inputs    :  slotStatus ;
    feedback  :   ;

    :---------------------------:
    inputs    :  slotStatus  ;
    feedback  :   ;
    operation :  dependentDecision name actionSpace ;
    outputs   :  reportMissingRequest ;
    returns   :  0 ;
    // Turn to payoffs later
    :---------------------------:

    outputs   :  reportMissingRequest ;
    returns   :   ;
  |]


-- ... continue with the different possible incidence reports

-- Aggregate reports
aggregateReports name actionSpaceRegistered actionSpaceMissingRequest aggregateReportFunction payoffFunction = [opengame|

    inputs    :  slotStatus ;
    feedback  :   ;

    :---------------------------:

    inputs    :  slotStatus  ;
    feedback  :  ;
    operation :  reportRegisteredProposer name actionSpaceRegistered ;
    outputs   :  reportRegister ;
    returns   :  ;

    inputs    :  slotStatus  ;
    feedback  :  ;
    operation :  reportMissingRequest name actionSpaceMissingRequest ;
    outputs   :  reportMissingRequest ;
    returns   :  ;

    inputs    :  reportRegister, reportMissingRequest  ;
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





