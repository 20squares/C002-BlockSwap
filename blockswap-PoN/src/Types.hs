{-# LANGUAGE DuplicateRecordFields #-}

module Types
  where

import OpenGames.Engine.Engine (Agent)

{-
Describes the types of the model and interfaces to the outside world
-}

-- Types for individual agents

type PrivateValue = Double        
type Payoff = Double

data Parameters = Parameters
  {parameter :: Double}
