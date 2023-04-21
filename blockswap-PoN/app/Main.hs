{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}


module Main
  (main) where

import OpenGames.Engine.Engine

import Analytics
import Parameterization
import Strategies
import Types

parameters = undefined

-- 1. main executable
main  = do
   putStrLn "Equilibrium Analytics: Honest Strategy"
   printEquilibriumReporterGame parameters1 fullStrategyHonest
   putStrLn "Equilibrium Analytics: False reporting strategy"
   printEquilibriumReporterGame parameters1 fullStrategyFalse
   putStrLn "Equilibrium Analytics: Honest Strategy -- too low payment"
   printEquilibriumReporterGame parameters2 fullStrategyHonest
