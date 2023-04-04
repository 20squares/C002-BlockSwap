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
Contains the full reporing model
-}

reporterDraft name aggregateReportFunction payoffFunction = aggregateReports name actionsGrievingProposer actionsMissingRequestProposer actionsMissingReplyProposer  actionsReplyTimeout actionsWrongSignature actionsMissingRequestBuilder actionsMissingReplyBuilder actionsLowPayment actionsFaultAndKicking aggregateReportFunction payoffFunction
