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

grievingStrategy ::
  Kleisli
     Stochastic
     (State, SlotID, ProposerAddr, BuilderAddr, Bool, Bool, Bool, Bool)
     (PenaltyReport PenaltyType)
grievingStrategy =
  Kleisli (\(state,slot,_,_,slotInPast,registeredProposer, missedPayment, demand) ->
             if    slotInPast                    == True
                && registeredProposer            == True
                && missedPayment                 == True
                && demand                        == True
                && checkBlocksForSlot state slot == True
                then playDeterministically $ NoPenalty 
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
                        then playDeterministically $ Penalty ProposerNoRequest
                        else playDeterministically NoPenalty
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
                _ -> if checkPayment state slot builderAddr == True
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
              if    slotInPast                    == True
                && verifyProposerKicking state proposerAddr == True
                  then playDeterministically NoPenalty
                  else playDeterministically $ Penalty Kicked
          )

submitReportStrategy ::
  Kleisli
     Stochastic
     (PenaltyReport PenaltyType, ProposerAddr, BuilderAddr)
     (SubmitReport AgentPenalized)
submitReportStrategy =
  Kleisli (\(report,_,_) ->
             case report of
                NoPenalty -> playDeterministically NoReport
                Penalty x -> playDeterministically $ matchPenaltyForReport x 
          )

submitFalseReportStrategy ::
  Kleisli
     Stochastic
     (PenaltyReport PenaltyType, ProposerAddr, BuilderAddr)
     (SubmitReport AgentPenalized)
submitFalseReportStrategy =
  Kleisli (\(report,_,_) ->
                playDeterministically $ SubmitReport Validator 0
          )

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



