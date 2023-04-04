{-# LANGUAGE DuplicateRecordFields #-}


module Types
  where

import OpenGames.Engine.Engine (Agent)


{-

INITIAL PROPOSAL
Describes the types of the model and interfaces to the outside world
-}
--------------
-- Actor types

-- Different actors
type Validator = Agent
type Builder   = Agent
type Reporter  = Agent

-- Types for individual agents
type PrivateValue = Double
type Payoff = Double

-----------------------
-- Infrastructure types
-- NOTE These are mainly placeholders to be extended later

type Signature = String

type TX = String

type Payment = Double

type PaymentPromise = Payment

type RegisteredProposer = Bool

-- We treat a block as a list of txs, plus the required payment as the last tx of the block
type Block = ([TX], Payment)

----------------------------------
-- What does the reporter observe?

-- Observes the builder action; the proposer action; and realized payment information (relevant for whether payment == payment promise and whether registered proposer received payment)
-- TODO WIP
-- TODO still need to think how to do that best as an interface to the actual contracts
-- TODO Also note the connection to aggregate report function that should also serve as a possible interface to the contract types
data SlotStatus = SlotStatus
     { proposerRegistered :: RegisteredProposer
     , builderAction :: BuilderAction
     , proposerAction :: ProposerAction
     , proposerStatus :: ProposerStatus
     , paymentReceived :: Maybe Payment
     } deriving (Eq,Ord,Show)

data BuilderAction = NotProposed | BlockProposed Block
  deriving (Eq,Ord,Show)

data ProposerAction = NoBlockRequested | BlockRequested Relay | BlockRequestedDelayed
  deriving (Eq,Ord,Show)

data ProposerStatus = BalanceBelow32 | Slashed | Withdraws
  deriving (Eq,Ord,Show)

data Relay = NotReplied | Replied Signature
  deriving (Eq,Ord,Show)

---------------------------------------------------------------------
-- This describes the kind of penalties which the reporter can report
-- Define the punishable events of the system that the reporter can report
-- NOTE we assume a hierarchy; top level reasons have precedence; this seems to be the case as suggested in the PoN doc


-- Check prerequisites
-- Is the proposer registered?
data ProposerGrieving = NotGrieving | Grieving
  deriving (Show,Eq)

-- Was the slot missed?
data SlotMissed = NotMissed | Missed
  deriving (Show,Eq,Ord)

-- Was there demand?
data Demand = NoDemand | Demand
  deriving (Show,Eq,Ord)

-- Penalty reporting
data PenaltyReport a = NoPenalty | Penalty a 
  deriving (Show,Eq)

-- Was there a request by the proposer?
data ProposerRequest = ProposerNoRequest | ProposerRequest
  deriving (Show,Eq)

-- Did the proposer reply?
data ProposerReply = ProposerNotReplied | ProposerReplied
  deriving (Show,Eq)

-- Did the proposer respond within time?
data ProposerRespondTime = NotWithinTime | WithinTime
  deriving (Show,Eq)

-- Was the signature verified?
data SignatureVerified = NotVerified | Verified
  deriving (Show,Eq)

-- Did the builder request it?
data BuilderRequest = BuilderNoRequest | BuilderRequest
  deriving (Show,Eq)

-- Was the signature verified?
data BuilderReply = BuilderNotReplied | BuilderReplied
  deriving (Show,Eq)

-- Was the signature verified?
data BuilderLowPayment = LowPayment | CorrectPayment
  deriving (Show,Eq)

-- Is the slot the same?
data SameSlot = NotSameSlot | IsSameSlot
  deriving (Show,Eq)

-- TODO Parameterized interface type for analysis
data Parameters = Parameters
  {parameter :: Double}

