{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Types
  where

import OpenGames.Engine.Engine (Agent)

import Optics

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

-- We treat a block as a list of txs, plus the required payment as the last tx of the block
type Block = ([TX], Payment)

----------------------------------
-- What does the reporter observe?

-- Observes the builder action; the proposer action; and realized payment information (relevant for whether payment == payment promise)
data SlotStatus = SlotStatus BuilderAction ProposerAction (Maybe Payment)

data BuilderAction = NotProposed | BlockProposed Block

data ProposerAction = NoBlockRequested | BlockRequested Relay | BlockRequestedDelayed

data Relay = NotReplied | Replied Signature

---------------------------------------------------------------------
-- This describes the kind of penalties which the reporter can report
-- Define the punishable events of the system that the reporter can report
-- NOTE we assume a hierarchy; top level reasons have precedence; this seems to be the case as suggested in the PoN doc


data PenaltyReport a = NoPenalty | Penalty a 
  deriving (Show,Eq)
makePrisms ''PenaltyReport

-- Is the proposer registered?
data ProposerRegistered a = NotRegistered | Registered a
  deriving (Show,Eq)
makePrisms ''ProposerRegistered

-- Was the slot missed?
data SlotMissed a = NotMissed | Missed a
  deriving (Show,Eq)
makePrisms ''SlotMissed

-- Was there demand?
data Demand a = NoDemand | Demand a
  deriving (Show,Eq)
makePrisms ''Demand

-- Was there a request by the proposer?
data ProposerRequest a = NoRequest | Request a
  deriving (Show,Eq)
makePrisms ''ProposerRequest

-- Did the proposer respond within time?
data ProposerRespondTime a = NotWithinTime | WithinTime a
  deriving (Show,Eq)
makePrisms ''ProposerRespondTime

-- Was the signature verified?
data SignatureVerified a = NotVerfied | Verified a
  deriving (Show,Eq)
makePrisms ''SignatureVerified

-- Is the slot the same?
data SameSlot a = NotSameSlot | IsSameSlot a
  deriving (Show,Eq)
makePrisms ''SameSlot

-- Did the builder request it?
data BuilderRequest = NotRequested | Requested
  deriving (Show,Eq)
makePrisms ''BuilderRequest

-- Parameterized interface type for analysis
data Parameters = Parameters
  {parameter :: Double}

