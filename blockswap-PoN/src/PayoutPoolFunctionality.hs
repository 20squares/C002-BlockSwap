{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}

module PayoutPoolFunctionality where

import Types

import OpenGames.Engine.Engine (Agent,Stochastic,uniformDist,pureAction,playDeterministically)

import Crem.BaseMachine(BaseMachine(..),ActionResult(..))
import Crem.StateMachine
import Crem.Topology(Topology(..))
import Data.Kind
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)

{-
Contains the main logic of the payout pool as far as it concerns the reporter
-}

-- TODO Build the payout pool in an extensionable fashion: We do this by building the functionality in terms of machines that get aggregated
-- TODO We need to focus on the reporter's actions.

-- This machine just updates the state of the payout pool after receiving a report submission
updateReportMachine :: StateMachine PayoutPool PayoutPool
updateReportMachine = undefined -- Basic $ BaseMachine undefined undefined undefined


topology = Topology [(ReportVertex,[PayoutPoolVertex])]

data VerticesReport = ReportVertex | PayoutPoolVertex

data ReportState (vertex :: VerticesReport) where
  ReportVertexState :: Report -> ReportState 'ReportVertex
  PayoutPoolVertextState :: PayoutPool -> ReportState 'PayoutPoolVertex

data InitialStateReport (state :: vertex -> Type) where
  InitialStateReport :: state vertex -> InitialStateReport state

action =  \s ->
           ActionResult (_,PayoutPoolVertextState PayoutPool)

-- This machine just updates the state of the payout pool after a reporter withdraws their payment
updateWithdrawReporterMachine :: StateMachine PayoutPool PayoutPool
updateWithdrawReporterMachine = undefined

-- Combine this into a machine which either accepts a report or a withdrawl
reporterMachine
  :: StateMachine (Either PayoutPool PayoutPool) (Either PayoutPool PayoutPool)
reporterMachine = Alternative updateReportMachine updateWithdrawReporterMachine
