module ActionSpaces where

import Types

{-
Describes the action space that the players have access to
This depends on the state of the system.
NOTE Here we include conditions of previous reporting actions. We follow the logic described in the PoN Reporting Rules doc
As a consequence, there should only be one place in which the report can emit a penalty
TODO check that this is indeed intended
NOTE The logic we include here can be independently changed from the information flow described in `Components.hs`
-}

-- Report proposer who is grieving the PoN
-- In order to report a penalty at this stage, the payment must be missed, demand must be confirmed
actionsGrievingProposer (_,_,_,_,slotInPast,registeredProposer, missedPayment, demand)
  |    slotInPast         == True
    && registeredProposer == True
    && missedPayment      == True
    && demand             == True  = [NoPenalty, Penalty Grieving]
  | otherwise                      = [NoPenalty]

-- Report missing request
-- In order to report a penalty at this stage, the payment must be missed, demand must be confirmed, the proposer must be registered
actionsMissingRequestProposer (_,_,_,_,slotInPast,registeredProposer, missedPayment, demand, reportGrieving)
  |    slotInPast         == True
    && registeredProposer == True
    && missedPayment      == True
    && demand             == True
    && reportGrieving     == NoPenalty = [NoPenalty, Penalty ProposerNoRequest]
  | otherwise                          = [NoPenalty]

-- Report no reply
-- In order to report a penalty at this stage, the payment must be missed, demand must be confirmed, the proposer must be registered, the proposer request must be made
actionsMissingReplyProposer (_,_,_,_,slotInPast,registeredProposer, missedPayment, demand, reportGrieving,reportMissingRequestProposer)
  |    slotInPast                   == True
    && registeredProposer           == True
    && missedPayment                == True
    && demand                       == True
    && reportGrieving               == NoPenalty
    && reportMissingRequestProposer == NoPenalty = [NoPenalty, Penalty ProposerNotReplied]
  | otherwise                                    = [NoPenalty]

-- Report delayed reply
-- In order to report a penalty at this stage, the payment must be missed, demand must be confirmed, the proposer must be registered, the proposer request must be made, the proposer must have replied
actionsReplyTimeout (_,_,_,_,slotInPast,registeredProposer, missedPayment, demand, reportGrieving,reportMissingRequestProposer,reportMissingReply)
  |    slotInPast                   == True
    && registeredProposer           == True
    && missedPayment                == True
    && demand                       == True
    && reportGrieving               == NoPenalty
    && reportMissingRequestProposer == NoPenalty
    && reportMissingReply           == NoPenalty = [NoPenalty, Penalty NotWithinTime]
  | otherwise                                    = [NoPenalty]

-- Report wrong signature
-- In order to report a penalty at this stage, the payment must be missed, demand must be confirmed, the proposer must be registered, the proposer request must be made, the proposer must have replied, the proposer must have replied in time
actionsWrongSignature (_,_,_,_,slotInPast,registeredProposer, missedPayment, demand, reportGrieving,reportMissingRequestProposer,reportMissingReply,reportReplyTimeOut)
  |    slotInPast                   == True
    && registeredProposer           == True
    && missedPayment                == True
    && demand                       == True
    && reportGrieving               == NoPenalty
    && reportMissingRequestProposer == NoPenalty
    && reportMissingReply           == NoPenalty
    && reportReplyTimeOut           == NoPenalty = [NoPenalty, Penalty NotVerified]
  | otherwise                                    = [NoPenalty]

-- Report missed slot due to no request sent to the relay by the builder
-- In order to report a penalty at this stage, the payment must be missed, demand must be confirmed, the proposer must be registered, the proposer request must be made, the proposer must have replied, the proposer must have replied in time, the proposer must have signed with the correct signature
actionsMissingRequestBuilder (_,_,_,_,slotInPast,registeredProposer, missedPayment, demand, reportGrieving,reportMissingRequestProposer,reportMissingReply,reportReplyTimeOut,reportSignature)
  |    slotInPast                   == True
    && registeredProposer           == True
    && missedPayment                == True
    && demand                       == True
    && reportGrieving               == NoPenalty
    && reportMissingRequestProposer == NoPenalty
    && reportMissingReply           == NoPenalty
    && reportReplyTimeOut           == NoPenalty
    && reportSignature              == NoPenalty = [NoPenalty, Penalty BuilderNoRequest]
  | otherwise                                    = [NoPenalty]

-- Report missed slot due to no reply sent to the relay by the builder
-- In order to report a penalty at this stage, the payment must be made, demand must be confirmed, the proposer must be registered, the proposer request must be made, the proposer must have replied, the proposer must have replied in time, the proposer must have signed with the correct signature, builder must have sent the request
actionsMissingReplyBuilder (_,_,_,_,slotInPast,registeredProposer, missedPayment, demand, reportGrieving,reportMissingRequestProposer,reportMissingReply,reportReplyTimeOut,reportSignature, reportMissingRequestBuilder)
  |    slotInPast                   == True
    && registeredProposer           == True
    && missedPayment                == True
    && demand                       == True
    && reportGrieving               == NoPenalty
    && reportMissingRequestProposer == NoPenalty
    && reportMissingReply           == NoPenalty
    && reportReplyTimeOut           == NoPenalty
    && reportSignature              == NoPenalty
    && reportMissingRequestBuilder  == NoPenalty = [NoPenalty, Penalty BuilderNotReplied ]
  | otherwise                                    = [NoPenalty]

-- Report low payment
-- In order to report a penalty at this stage, the payment must be made, demand must be confirmed, the proposer must be registered, the proposer request must be made, the proposer must have replied, the proposer must have replied in time, the proposer must have signed with the correct signature, builder must have sent the request
actionsLowPayment (_,_,_,_,slotInPast,registeredProposer, missedPayment, demand, reportGrieving,reportMissingRequestProposer,reportMissingReply,reportReplyTimeOut,reportSignature, reportMissingRequestBuilder,reportMissingReplyBuilder)
  |    slotInPast                   == True
    && registeredProposer           == True
    && missedPayment                == False
    && demand                       == True
    && reportGrieving               == NoPenalty
    && reportMissingRequestProposer == NoPenalty
    && reportMissingReply           == NoPenalty
    && reportReplyTimeOut           == NoPenalty
    && reportSignature              == NoPenalty
    && reportMissingRequestBuilder  == NoPenalty
    && reportMissingReplyBuilder    == NoPenalty = [NoPenalty, Penalty LowPayment]
  | otherwise                                    = [NoPenalty]


-- Report kicking condition
-- In order to report a proposer, the proposer must be registered
-- TODO still unclear whether there is an additional costs/penalty 
actionsFaultAndKicking (_,_,_,_,registeredProposer)
  | registeredProposer == True = [NoPenalty, Penalty Kicked]
  | otherwise          = [NoPenalty]

-- Submit on-chain report
actionsOnChainReport penaltyValidator penaltyBuilder penaltyValidatorKicking (_,addrProposer,addrBuilder) = [NoReport, SubmitReport Validator penaltyValidator, SubmitReport Builder penaltyBuilder, SubmitReport ValidatorKicked penaltyValidatorKicking]

