{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}


module Types
  where

import OpenGames.Engine.Engine (Agent)

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Optics.TH

{-

INITIAL PROPOSAL
Describes the types of the model and interfaces to the outside world
-}
----------------
-- 1 Actor types
----------------

-- Different actors
type Validator = Agent
type Builder   = Agent

-- Types for individual agents
type PrivateValue = Double
type Payoff = Double

-------------------------
-- 2 Infrastructure types
-------------------------

-- NOTE These are mainly placeholders to be extended later

type Signature = String

type TX = String

type Payment = Double

type PaymentPromise = Payment

type SlotID = Integer

type BlockID = Integer

type ProposerAddr = String

type BuilderAddr = String

type ReporterAddr = String

type PayoutPoolAddr = String

type ReporterRegistryAddr = String

type RelayerID = String

type ETH = Double

type BlockHeader = String

type Msg = String

type Time = Integer

type Epoch = Integer

type PayoutCycles = Integer

type RPBS = String -- TODO we just use this as a place-holder

type ChoiceFunction = String -- TODO we just use this as a place-holder

type S = String -- TODO Check on type used in documentation

type ReporterBonus = Payoff

type ReporterMalus = Payoff

type PenaltyAmount = Double

---------------------------------
-- 3 Types for bids, auctions etc
---------------------------------

data Bid = Bid
   { builder :: BuilderAddr
   , relayer :: RelayerID
   , promise :: ETH -- FIXME (in documentation this is stated as a natural number?)
   , slotBid :: SlotID
   , crytogr :: (BlockHeader,RPBS)
   } deriving (Eq,Ord,Show)

type Auction = Map SlotID [Bid] -- FIXME (check documentation; is that model correct?)

-- Information held in a single relayer
data Relayer = Relayer
   { relayer        :: RelayerID
   , choiceFunction :: ChoiceFunction
   , schedule       :: Map Time (BidInTime, SignedInTime, BroadcastInTime)
   , checkRPBS      :: Map (RPBS, ETH, BuilderAddr) Bool
   , validBid       :: Map Bid Bool
   , signedPoP      :: Map Bid S
   , signedHeader   :: Map Bid S
   , request        :: Map SlotID Bool
   , receive        :: Map SlotID S
   } deriving (Eq,Ord,Show)


-----------------
-- 4 Status types
-----------------

data ProposerStatus = ProposerUnregistered | ProposerRegistered | ProposerActive | ProposerExitPending | ProposerExited | ProposerKicked
   deriving (Eq,Ord,Show)

-- TODO using a simplification for the schedule information
type BidInTime       = Bool
type SignedInTime    = Bool
type BroadcastInTime = Bool

------------------------------------------------------
-- 5 Define state variables observable by the reporter
------------------------------------------------------

-- Data on chain
data StateOnChain = StateOnChain
     { _slotId          :: SlotID
     , _proposerForSlot :: Map SlotID ProposerAddr
     , _proposerStake   :: Map ProposerAddr ETH
     , _balanceAccount  :: Map ProposerAddr ETH
     , _slotFee         :: Map SlotID ETH
     , _signedBlocks    :: Map SlotID Integer
     , _block           :: BlockID
     , _msg             :: Msg
     , _payoutPool      :: PayoutPool
     } deriving (Eq,Ord,Show)

-- Data on chain specific to PoN
data StatePoNOnChain = StatePoNOnChain
    { _proposerStatus       :: Map ProposerAddr ProposerStatus
    , _isBuilderOperational :: Map BuilderAddr Bool
    , _paidInSlot           :: Map (SlotID,BuilderAddr) ETH
    } deriving (Eq,Ord,Show)

-- Data
-- Complete state
data State = State
   { _stateOnChain    :: StateOnChain
   , _statePoNOnChain :: StatePoNOnChain
   , _stateOffChain   :: ([Relayer],Auction)
   } deriving (Eq,Ord,Show)

--------------------------------------------
-- 6 Penalties which the reporter can report
--------------------------------------------

-- Define the punishable events of the system that the reporter can report
-- This is internal to the reporter; not exposed to the outside sysmte directly
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

-- Verification status of report
-- NOTE we allow for the verification to indicate the agent who was correctly (or falsely) reported
data ReportVerification a =  ReportCorrect a | ReportFalse a
  deriving (Show,Eq,Ord)

-- For on-chain report identify players to be penalized
data AgentPenalized = Validator | Builder | ValidatorKicked
  deriving (Show,Eq
           ,Ord)

-- Fix payoffs for reporter
-- NOTE we include payoff components for all possible eventualities; some of them might not be needed
data ReporterPayoffParameters = ReporterPayoffParameters
  { reportCorrectValidator       :: ReporterBonus
  , reportCorrectBuilder         :: ReporterBonus
  , reportCorrectValidatorKicked :: ReporterBonus
  , reportFalseValidator         :: ReporterMalus
  , reportFalseBuilder           :: ReporterMalus
  , reportFalseValidatorKicked   :: ReporterMalus
  } deriving (Show,Eq,Ord)

----------------
-- 7 Payout pool
----------------

-- 7.1 Reporter
data Reporter = Reporter
  { _rewards           :: ETH
  , _isActive          :: Bool
  , _isRageQuitted     :: Bool
  , _lastReportedBlock :: Maybe BlockID
  } deriving (Show,Eq,Ord)

-- All reporters
type Reporters = [Reporter]

-- 7.2. Proposer
-- NOTE: incomplete and to be augmented and changed later when proposer is addressed
data ProposerType = ProposerType
  {_reportCount :: Integer
  } deriving (Show,Eq,Ord)

-- 7.3. Builder
-- NOTE: incomplete and to be augmented and changed later when proposer is addressed
data BuilderType =BuilderType
  {_stake :: ETH
  } deriving (Show,Eq,Ord)

-- 7.4 Report
data Report = Report
  { _proposer :: ProposerAddr
  , _builder  :: BuilderAddr
  , _amount   :: PenaltyAmount
  , _slotId   :: SlotID
  , _blockId  :: BlockID
  , _penaltyType :: AgentPenalized
  } deriving (Show,Eq,Ord)

-- 7.5 Payout pool
-- NOTE We only include fields that of relevance for the reporter
data PayoutPool = PayoutPool
  { _payoutPoolAddr       :: PayoutPoolAddr
  , _reporterRegistry     :: Map ReporterAddr Reporter
  , _reporterRegistryAddr :: ReporterRegistryAddr
  , _proposerRegistry     :: Map ProposerAddr ProposerType
  , _builderRegistry      :: Map BuilderAddr BuilderType
  , _reportsSlotsInUse    :: Map SlotID Bool
  , _maintenanceBalance   :: ETH
  , _kickThreshold        :: Integer
  , _payoutCycleLength    :: PayoutCycles
  , _deploymentEpoch      :: Epoch
  , _reporterPayoutDelay  :: Integer
  } deriving (Show,Eq,Ord)

-- Submit a report

-- Withdraw funds
newtype WithdrawFunds a = WithdrawFunds a

---------------
-- 8 Parameters
---------------

-- Parameters for context
data ContextParameters = ContextParameters
  { state        :: State        -- ^ Current state
  , slot         :: SlotID       -- ^ Slot to be reported
  , proposerAddr :: ProposerAddr -- ^ Proposer of slot to be reported
  , builderAddr  :: BuilderAddr  -- ^ Builder of slot to be reported
  } deriving (Show,Eq,Ord)

-- Parameterized interface type for analysis
data Parameters = Parameters
  { reporterName             :: Agent
  , penaltyValidator         :: PenaltyAmount
  , penaltyBuilder           :: PenaltyAmount
  , penaltyValidatorKicking  :: PenaltyAmount
  , reporterPayoffParameters :: ReporterPayoffParameters
  , contextParameters        :: ContextParameters
  }
  deriving (Show,Eq,Ord)


deriving instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m, Show n, Show o, Show p) => Show (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)
deriving instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m, Show n, Show o, Show p, Show q) => Show (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q)
-- ^ worshipping GHC 

---------
-- Optics
---------

makeLenses ''State
makeLenses ''StatePoNOnChain
makeLenses ''StateOnChain
makeLenses ''PayoutPool
makeLenses ''Reporter
makeLenses ''ProposerType
makeLenses ''BuilderType
