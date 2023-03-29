{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Strategies
  where


import Types

import OpenGames.Engine.Engine

import Data.List (maximumBy)
import Data.Ord (comparing)

{-
Defines the strategies
-}
