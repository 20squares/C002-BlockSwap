{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Strategies
  where

import Payoffs
import SupportFunctions
import Types

import OpenGames.Engine.Engine

import Data.List (maximumBy)
import Data.Ord (comparing)

{-
Defines the strategies
-}


--------------------------------------
-- 1 Strategies for individual reports
--------------------------------------

grievingStrategy ::
  Kleisli
     Stochastic
     (State, SlotID, ProposerAddr, BuilderAddr, Bool, Bool, Bool, Bool)
     (PenaltyReport PenaltyType)
grievingStrategy =
  Kleisli (\(state,slot,_,_,slotInPast,registeredProposer, missedPayment, demand) ->
             if    slotInPast
                && registeredProposer            
                && missedPayment                 
                && demand                        
                && checkBlocksForSlot state slot 
                then playDeterministically NoPenalty
                else playDeterministically $ Penalty Grieving
          )


missingRequestStrategy ::
  Kleisli
     Stochastic
     (State, SlotID, ProposerAddr, BuilderAddr, Bool, Bool, Bool, Bool, PenaltyReport PenaltyType)
     (PenaltyReport PenaltyType)
missingRequestStrategy =
  Kleisli (\(state,slot,_,_,slotInPast,registeredProposer, missedPayment, demand, grievingPenalty) ->
              case grievingPenalty of
                Penalty x -> playDeterministically $ Penalty x
                _ -> if checkProposerRequest state slot
                        then playDeterministically NoPenalty
                        else playDeterministically $ Penalty ProposerNoRequest
          )

-- NOTE: There seems to be no way to distinguish the reply errors
missingReplyStrategy1 ::
  Kleisli
     Stochastic
     (State, SlotID, ProposerAddr, BuilderAddr, Bool, Bool, Bool, Bool, PenaltyReport PenaltyType, PenaltyReport PenaltyType)
     (PenaltyReport PenaltyType)
missingReplyStrategy1 =
  Kleisli (\(state,slot,_,_,slotInPast,registeredProposer, missedPayment, demand, grievingPenalty, missingRequest) ->
              case missingRequest of
                Penalty x -> playDeterministically $ Penalty x
                _ -> if checkProposerReplied state slot
                        then playDeterministically NoPenalty
                        else playDeterministically $ Penalty ProposerNotReplied
          )

missingReplyStrategy2 ::
  Kleisli
     Stochastic
     (State, SlotID, ProposerAddr, BuilderAddr, Bool, Bool, Bool, Bool, PenaltyReport PenaltyType, PenaltyReport PenaltyType, PenaltyReport PenaltyType)
     (PenaltyReport PenaltyType)
missingReplyStrategy2 =
  Kleisli (\(_,_,_,_,slotInPast,registeredProposer, missedPayment, demand, grievingPenalty, missingRequest, missingReply1) ->
              case missingReply1 of
                Penalty x -> playDeterministically $ Penalty NotWithinTime
                _ -> playDeterministically NoPenalty
          )

missingReplyStrategy3 ::
  Kleisli
     Stochastic
     (State, SlotID, ProposerAddr, BuilderAddr, Bool, Bool, Bool, Bool, PenaltyReport PenaltyType, PenaltyReport PenaltyType, PenaltyReport PenaltyType, PenaltyReport PenaltyType)
     (PenaltyReport PenaltyType)
missingReplyStrategy3 =
  Kleisli (\(_,_,_,_,slotInPast,registeredProposer, missedPayment, demand, grievingPenalty, missingRequest, missingReply1, missingReply2) ->
              case missingReply2 of
                Penalty x -> playDeterministically $ Penalty NotVerified
                _ -> playDeterministically NoPenalty
          )

-- NOTE We cannot distinguish the next two type of reports
missingRequestBuilder1 ::
  Kleisli
     Stochastic
     (State, SlotID, ProposerAddr, BuilderAddr, Bool, Bool, Bool, Bool, PenaltyReport PenaltyType, PenaltyReport PenaltyType, PenaltyReport PenaltyType, PenaltyReport PenaltyType, PenaltyReport PenaltyType)
     (PenaltyReport PenaltyType)
missingRequestBuilder1 =
  Kleisli (\(state,slot,_,builderAddr,slotInPast,registeredProposer, missedPayment, demand, grievingPenalty, missingRequest, missingReply1, missingReply2, missingReply3) ->
              case missingReply3 of
                Penalty x -> playDeterministically $ Penalty x
                _ -> if checkPayment state slot builderAddr
                        then playDeterministically NoPenalty
                        else playDeterministically $ Penalty BuilderNoRequest
          )

missingRequestBuilder2 ::
  Kleisli
     Stochastic
     (State, SlotID, ProposerAddr, BuilderAddr, Bool, Bool, Bool, Bool, PenaltyReport PenaltyType, PenaltyReport PenaltyType, PenaltyReport PenaltyType, PenaltyReport PenaltyType, PenaltyReport PenaltyType, PenaltyReport PenaltyType)
     (PenaltyReport PenaltyType)
missingRequestBuilder2 =
  Kleisli (\(state,slot,_,builderAddr,slotInPast,registeredProposer, missedPayment, demand, grievingPenalty, missingRequest, missingReply1, missingReply2, missingReply3, missingRequestBuilder1) ->
              case missingRequestBuilder1 of
                Penalty x -> playDeterministically $ Penalty BuilderNotReplied
                _ -> playDeterministically NoPenalty
          )

lowPaymentBuilder ::
  Kleisli
     Stochastic
     (State, SlotID, ProposerAddr, BuilderAddr, Bool, Bool, Bool, Bool, PenaltyReport PenaltyType, PenaltyReport PenaltyType, PenaltyReport PenaltyType, PenaltyReport PenaltyType, PenaltyReport PenaltyType, PenaltyReport PenaltyType, PenaltyReport PenaltyType)
     (PenaltyReport PenaltyType)
lowPaymentBuilder =
  Kleisli (\(state,slot,_,builderAddr,slotInPast,registeredProposer, missedPayment, demand, grievingPenalty, missingRequest, missingReply1, missingReply2, missingReply3, missingRequestBuilder1, missingRequestBuilder2) ->
              case missingRequestBuilder2 of
                Penalty x -> playDeterministically $ Penalty x
                _ -> if checkBuilderPayment state slot builderAddr
                        then playDeterministically NoPenalty
                        else playDeterministically $ Penalty LowPayment
          )

kickingStrategy ::
  Kleisli
     Stochastic
     (State, SlotID, ProposerAddr, BuilderAddr, Bool)
     (PenaltyReport PenaltyType)
kickingStrategy =
  Kleisli (\(state,slot,proposerAddr,builderAddr,slotInPast) ->
              if    slotInPast                    
                && verifyProposerKicking state proposerAddr
                  then playDeterministically $ Penalty Kicked
                  else playDeterministically NoPenalty
          )

---------------------------------
-- 2 Overall reporting strategies
---------------------------------

submitReportStrategy ::
  Kleisli
     Stochastic
     (State, SlotID, ProposerAddr, BuilderAddr, PenaltyReport PenaltyType, PenaltyReport PenaltyType)
     (SubmitReport Report)
submitReportStrategy =
  Kleisli (\(_,slotId,proposerAdr',builderAdr',report,kickingReport) ->
             case report of
                NoPenalty ->
                  case kickingReport of
                     NoPenalty -> playDeterministically NoReport
                     Penalty x -> playDeterministically $ matchPenaltyForReport slotId proposerAdr' builderAdr' x
                Penalty x -> playDeterministically $ matchPenaltyForReport slotId proposerAdr' builderAdr' x
          )

submitFalseReportStrategy ::
  Kleisli
     Stochastic
     (State, SlotID, ProposerAddr, BuilderAddr, PenaltyReport PenaltyType, PenaltyReport PenaltyType)
     (SubmitReport Report)
submitFalseReportStrategy =
  Kleisli (\(_,slotId,proposerAdr',builderAdr',_,_) ->
                let report = Report
                        { _proposer = proposerAdr'
                        , _builder  = builderAdr'
                        , _amount   = 0
                        , _slotId   = 1
                        , _blockId  = 0
                        , _penaltyType = Validator
                        }
                in playDeterministically $ SubmitReport report
          )

noSubmitReportStrategy ::
  Kleisli
     Stochastic
     (State, SlotID, ProposerAddr, BuilderAddr, PenaltyReport PenaltyType, PenaltyReport PenaltyType)
     (SubmitReport Report)
noSubmitReportStrategy =
  Kleisli (\(_,slotId,proposerAdr',builderAdr',report,kickingReport) ->
              playDeterministically NoReport
          )

-------------------------
-- 3 Full strategy tuples
-------------------------

-- Full reporter strategy
fullStrategyHonest =
  grievingStrategy
  ::- missingRequestStrategy
  ::- missingReplyStrategy1
  ::- missingReplyStrategy2
  ::- missingReplyStrategy3
  ::- missingRequestBuilder1
  ::- missingRequestBuilder2
  ::- lowPaymentBuilder
  ::- kickingStrategy
  ::- submitReportStrategy
  ::- Nil

-- Full reporter strategy when report wrong 
fullStrategyFalse =
  grievingStrategy
  ::- missingRequestStrategy
  ::- missingReplyStrategy1
  ::- missingReplyStrategy2
  ::- missingReplyStrategy3
  ::- missingRequestBuilder1
  ::- missingRequestBuilder2
  ::- lowPaymentBuilder
  ::- kickingStrategy
  ::- submitFalseReportStrategy
  ::- Nil

-- Stupid strategy, choose not to report abnormal behavior
noReportStrategy =
  grievingStrategy
  ::- missingRequestStrategy
  ::- missingReplyStrategy1
  ::- missingReplyStrategy2
  ::- missingReplyStrategy3
  ::- missingRequestBuilder1
  ::- missingRequestBuilder2
  ::- lowPaymentBuilder
  ::- kickingStrategy
  ::- noSubmitReportStrategy
  ::- Nil

