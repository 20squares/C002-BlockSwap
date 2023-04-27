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
  , reportFalseValidator         = 0
  , reportFalseBuilder           = 0
  , reportFalseValidatorKicked   = 0
  }

stateOnChain1 = StateOnChain
     { _slotId          = 5
     , _proposerForSlot = M.fromList [(1,"proposer1"),(2,"proposer2"),(3,"proposer3"),(4,"proposer1"),(5,"proposer2")]
     , _proposerStake   = M.fromList [("proposer1",33),("proposer2",40),("proposer3",15)]
     , _balanceAccount  = M.fromList [("proposer1",40),("proposer2",50),("proposer3",25)]
     , _slotFee         = M.fromList [(1,5),(2,5),(3,5),(4,5),(5,5)]
     , _signedBlocks    = M.fromList [(1,11),(2,12),(3,13),(4,14),(5,15)]
     , _block           = 5 -- FIXME Check this entry and meaning
     , _msg             = "Msg"
     , _payoutPool      = payoutPool1
     }

statePoNOnChain1 = StatePoNOnChain
    { _proposerStatus       = M.fromList [("proposer1",ProposerRegistered),("proposer2",ProposerRegistered),("proposer3",ProposerExited)]
    , _isBuilderOperational = M.fromList [("builder1", True),("builder2", True), ("builder3", True)]
    , _paidInSlot           = M.fromList [((1,"builder1"), 5),((2,"builder2"), 5), ((3,"builder3"), 5), ((4,"builder1"), 5), ((5,"builder1"), 5)] 
    }

-- Too small payment 
statePoNOnChain2 = StatePoNOnChain
    { _proposerStatus       = M.fromList [("proposer1",ProposerRegistered),("proposer2",ProposerRegistered),("proposer3",ProposerExited)]
    , _isBuilderOperational = M.fromList [("builder1", True),("builder2", True), ("builder3", True)]
    , _paidInSlot           = M.fromList [((1,"builder1"), 5),((2,"builder2"), 1), ((3,"builder3"), 5), ((4,"builder1"), 5), ((5,"builder1"), 5)] 
    }

-- No payment received
-- TODO Fix lookup condition
statePoNOnChain3 = StatePoNOnChain
    { _proposerStatus       = M.fromList [("proposer1",ProposerRegistered),("proposer2",ProposerRegistered),("proposer3",ProposerExited)]
    , _isBuilderOperational = M.fromList [("builder1", True),("builder2", True), ("builder3", True)]
    , _paidInSlot           = M.fromList [((1,"builder1"), 5), ((3,"builder3"), 5), ((4,"builder1"), 5), ((5,"builder1"), 5)] 
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

-- Too small payment
state2 = State stateOnChain1 statePoNOnChain2 stateOffChain1

-- No payment received
state3 = State stateOnChain1 statePoNOnChain3 stateOffChain1

contextParameters1 = ContextParameters
  { state        = state1
  , slot         = 2
  , proposerAddr = "proposer2"
  , builderAddr  = "builder2"
  }

-- Too small payment
contextParameters2 = ContextParameters
  { state        = state2
  , slot         = 2
  , proposerAddr = "proposer2"
  , builderAddr  = "builder2"
  }

-- No payment received
contextParameters3 = ContextParameters
  { state        = state3
  , slot         = 2
  , proposerAddr = "proposer2"
  , builderAddr  = "builder2"
  }

reporter1 = Reporter
  { rewards           = 0
  , isActive          = True
  , isRageQuitted     = False
  , lastReportedBlock = Nothing
  }

reporter2 = Reporter
  { rewards           = 0
  , isActive          = True
  , isRageQuitted     = False
  , lastReportedBlock = Nothing
  }

reporterRegistry1 = M.fromList [("reporter1",reporter1),("reporter2",reporter2)]

proposerRegistry1 = M.fromList [("proposer1", ProposerType 0),("proposer2", ProposerType 0),("proposer3", ProposerType 0)]

payoutPool1 = PayoutPool
  { _payoutPoolAddr       = "payoutPoolAddr"
  , _reporterRegistry     = reporterRegistry1
  , _reporterRegistryAddr = "reporterRegistryAddr1"
  , _proposerRegistry     = proposerRegistry1
  , _reportsSlotsInUse    = M.empty
  , _maintenaceBalance    = 2
  , _kickThreshold        = 1
  , _payoutCycleLength    = 2
  , _deploymentEpoch      = 0
  }

parameters1 = Parameters
  { reporterName             = "reporter"
  , penaltyValidator         = 10
  , penaltyBuilder           = 10
  , penaltyValidatorKicking  = 10
  , reporterPayoffParameters = reporterPayoffParameters1
  , contextParameters        = contextParameters1
  }

parameters2 = Parameters
  { reporterName             = "reporter"
  , penaltyValidator         = 10
  , penaltyBuilder           = 10
  , penaltyValidatorKicking  = 10
  , reporterPayoffParameters = reporterPayoffParameters1
  , contextParameters        = contextParameters2
  }

parameters3 = Parameters
  { reporterName             = "reporter"
  , penaltyValidator         = 10
  , penaltyBuilder           = 10
  , penaltyValidatorKicking  = 10
  , reporterPayoffParameters = reporterPayoffParameters1
  , contextParameters        = contextParameters3
  }
