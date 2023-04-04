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
actionsGrievingProposer (_,registeredProposer, missedPayment, demand)
  | registeredProposer == True && missedPayment == True && demand == True  = [NoPenalty, Penalty Grieving]
  | otherwise                                                              = [NoPenalty]

-- Report missing request
-- In order to report a penalty at this stage, the payment must be missed, demand must be confirmed, the proposer must be registered
actionsMissingRequest (_,registeredProposer, missedPayment, demand, reportGrieving)
  | registeredProposer == True && missedPayment == True && demand == True && reportGrieving == NoPenalty = [NoPenalty, Penalty ProposerNoRequest]
  | otherwise = [NoPenalty]

-- Report no reply
-- In order to report a penalty at this stage, the payment must be missed, demand must be confirmed, the proposer must be registered, the proposer request must be made
actionsMissingReplyProposer (_,registeredProposer, missedPayment, demand, reportGrieving,reportMissingRequestProposer)
  |    registeredProposer           == True
    && missedPayment                == True
    && demand                       == True
    && reportGrieving               == NoPenalty
    && reportMissingRequestProposer == NoPenalty = [NoPenalty, Penalty ProposerNotReplied]
  | otherwise                                    = [NoPenalty]

-- Report delayed reply
-- In order to report a penalty at this stage, the payment must be missed, demand must be confirmed, the proposer must be registered, the proposer request must be made, the proposer must have replied
actionsReplyTimeout (_,registeredProposer, missedPayment, demand, reportGrieving,reportMissingRequestProposer,reportMissingReply)
  |    registeredProposer           == True
    && missedPayment                == True
    && demand                       == True
    && reportGrieving               == NoPenalty
    && reportMissingRequestProposer == NoPenalty
    && reportMissingReply           == NoPenalty = [NoPenalty, Penalty NotWithinTime]
  | otherwise                                    = [NoPenalty]

-- Report wrong signature
-- In order to report a penalty at this stage, the payment must be missed, demand must be confirmed, the proposer must be registered, the proposer request must be made, the proposer must have replied, the proposer must have replied in time
actionsWrongSignature (_,registeredProposer, missedPayment, demand, reportGrieving,reportMissingRequestProposer,reportMissingReply,reportReplyTimeOut)
  |    registeredProposer           == True
    && missedPayment                == True
    && demand                       == True
    && reportGrieving               == NoPenalty
    && reportMissingRequestProposer == NoPenalty
    && reportMissingReply           == NoPenalty
    && reportReplyTimeOut           == NoPenalty = [NoPenalty, Penalty NotVerified]
  | otherwise                                    = [NoPenalty]

-- Report missed slot
-- In order to report a penalty at this stage, the payment must be missed, demand must be confirmed, the proposer must be registered, the proposer request must be made, the proposer must have replied, the proposer must have replied in time, the proposer must have signed with the correct signature
-- NOTE the logic; 
actionsRequestBuilder (_,registeredProposer, missedPayment, demand, reportGrieving,reportMissingRequestProposer,reportMissingReply,reportReplyTimeOut,reportSignature)
  |    registeredProposer           == True
    && missedPayment                == True
    && demand                       == True
    && reportGrieving               == NoPenalty
    && reportMissingRequestProposer == NoPenalty
    && reportMissingReply           == NoPenalty
    && reportReplyTimeOut           == NoPenalty
    && reportSignature              == NoPenalty = [NoPenalty, Penalty BuilderNoRequest]
  | otherwise                                    = [NoPenalty]

 -- Report low payment
-- In order to report a penalty at this stage, the payment must be made, demand must be confirmed, the proposer must be registered, the proposer request must be made, the proposer must have replied, the proposer must have replied in time, the proposer must have signed with the correct signature
-- NOTE the logic; 
actionsMissingReplyBuilder (_,registeredProposer, missedPayment, demand, reportGrieving,reportMissingRequestProposer,reportMissingReply,reportReplyTimeOut,reportSignature, reportMissingRequestBuilder)
  |    registeredProposer           == True
    && missedPayment                == False
    && demand                       == True
    && reportGrieving               == NoPenalty
    && reportMissingRequestProposer == NoPenalty
    && reportMissingReply           == NoPenalty
    && reportReplyTimeOut           == NoPenalty
    && reportSignature              == NoPenalty
    && reportMissingRequestBuilder  == NoPenalty = [NoPenalty, Penalty BuilderNotReplied ]
  | otherwise                                    = [NoPenalty]


