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
-- Penalty reporting
data PenaltyReport a = NoPenalty | Penalty a
  deriving (Show,Eq,Ord)

data PenaltyType =
       Grieving           -- ^ Did the proposer receive payment outside of the PoN
     | ProposerNoRequest  -- ^ Was there a request by the proposer?
     | ProposerNotReplied -- ^ Did the proposer reply?
     | NotWithinTime      -- ^ Did the proposer respond within time?
     | NotVerified        -- ^ Was the signature verified?
     | BuilderNoRequest   -- ^ Did the builder request it?
     | BuilderNotReplied  -- ^ Was the signature verified?
     | LowPayment         -- ^ Was payment too low?
     | Kicked             -- ^ Does the proposer get kicked? TODO We need to check that; conditions are not 100% clear
  deriving (Show,Eq,Ord)

-- On-chain component fixing the fault type a reporter can submit
data SubmitReport a = NoReport | SubmitReport a
  deriving (Show,Eq,Ord)

-- For on-chain report identify players to be penalized
data AgentsPenalized = Validator | Builder | ValidatorKicked
  deriving (Show,Eq,Ord)



-- TODO Parameterized interface type for analysis
data Parameters = Parameters
  {parameter :: Double}

