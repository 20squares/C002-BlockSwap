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
   putStrLn "Equilibrium Analytics:"
   printEquilibriumReporterGame parameters1 fullStrategy
