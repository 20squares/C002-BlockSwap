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

-- 1. main executable
main  = do
   putStrLn "Sanity check [EXPECTED TO FAIL]"
   putStrLn "Equilibrium Analytics: Choosing not to report a misbehavior"
   printEquilibriumReporterGame parameters2 noReportStrategy
   putStrLn "-------------------"
   putStrLn "Honest strategies"
   putStrLn "Equilibrium Analytics: Honest Strategy"
   printEquilibriumReporterGame parameters1 fullStrategyHonest
   putStrLn "Equilibrium Analytics: Honest Strategy -- too low payment"
   printEquilibriumReporterGame parameters2 fullStrategyHonest
   putStrLn "Equilibrium Analytics: Honest Strategy -- no payment"
   printEquilibriumReporterGame parameters3 fullStrategyHonest
   putStrLn "-------------------"
   putStrLn "False reporting strategies" 
   putStrLn "Equilibrium Analytics: False reporting strategy, no fee case"
   printEquilibriumReporterGame parameters1 fullStrategyFalse
   putStrLn "Equilibrium Analytics: False reporting strategy, case with fees [EXPECTED TO FAIL]"
   printEquilibriumReporterGame parameters1a fullStrategyFalse


