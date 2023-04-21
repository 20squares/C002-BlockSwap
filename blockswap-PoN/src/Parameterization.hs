{-# LANGUAGE DuplicateRecordFields #-}

module Parameterization
  where

import Model
import Types

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)



{-
Defines the main parameterizations used in the analysis
-}

reporterPayoffParameters1 = ReporterPayoffParameters
  { reportCorrectValidator       = 10
  , reportCorrectBuilder         = 10
  , reportCorrectValidatorKicked = 10
  , reportFalseValidator         = 10
  , reportFalseBuilder           = 10
  , reportFalseValidatorKicked   = 10
  }

stateOnChain1 = StateOnChain
     { slotId          = 5
     , proposerForSlot = M.fromList [(1,"proposer1"),(2,"proposer2"),(3,"proposer3"),(4,"proposer1"),(5,"proposer2")]
     , proposerStake   = M.fromList [("proposer1",30),("proposer2",40),("proposer3",15)]
     , balanceAccount  = M.fromList [("proposer1",40),("proposer2",50),("proposer3",25)]
     , slotFee         = M.fromList [(1,5),(2,5),(3,5),(4,5),(5,5)]
     , signedBlocks    = M.fromList [(1,11),(2,12),(3,13),(4,14),(5,15)]
     }

statePoNOnChain1 = StatePoNOnChain
    { proposerStatus       = M.fromList [("proposer1",ProposerRegistered),("proposer2",ProposerRegistered),("proposer3",ProposerExited)]
    , isBuilderOperational = M.fromList [("builder1", True),("builder2", True), ("builder3", True)]
    , paidInSlot           = M.fromList [((1,"builder1"), 5),((2,"builder2"), 5), ((3,"builder3"), 5), ((4,"builder1"), 5), ((5,"builder1"), 5)] 
    }

relay1 = Relayer
   { relayer        = "1"
   , choiceFunction = "choiceFunction"
   , schedule       = M.empty
   , checkRPBS      = M.empty
   , validBid       = M.fromList [(bid1,True),(bid2,True),(bid3,True),(bid4,True),(bid5,True)]
   , signedPoP      = M.empty
   , signedHeader   = M.empty
   , request        = M.fromList [(1,True),(2,True),(3,True),(4,True),(5,True)]
   , receive        = M.fromList [(1,"proposer1"),(2,"proposer2"),(3,"proposer3"),(4,"proposer1"),(5,"proposer2")]
   }

relays1 = [relay1]

bid1 = Bid "builder1" "1" 5 1 ("empty","empty")
bid2 = Bid "builder2" "1" 5 2 ("empty","empty")
bid3 = Bid "builder3" "1" 5 3 ("empty","empty")
bid4 = Bid "builder1" "1" 5 4 ("empty","empty")
bid5 = Bid "builder2" "1" 5 5 ("empty","empty")

auction1 = M.fromList
  [ (1,[bid1])
  , (2,[bid2])
  , (3,[bid3])
  , (4,[bid4])
  , (5,[bid5])]

stateOffChain1 = (relays1,auction1)

state1 = State stateOnChain1 statePoNOnChain1 stateOffChain1

contextParameters1 = ContextParameters
  { ctxState        = state1
  , ctxSlotId       = 2
  , ctxProposerAddr = "proposer2"
  , ctxBuilderAddr  = "builder2"
  } 

parameters1 = Parameters
  { reporterName             = "reporter"
  , penaltyValidator         = 10
  , penaltyBuilder           = 10
  , penaltyValidatorKicking  = 10
  , reporterPayoffParameters = reporterPayoffParameters1
  , payoutPoolParameter      = PayoutPool 2
  , contextParameters        = contextParameters1
  }
