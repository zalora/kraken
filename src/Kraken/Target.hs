{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}

-- | Implements a simple target.

module Kraken.Target (
    Target(..),
    TargetName(..),
    TargetM,
    Monitor(..),
    fromMonitor,
  ) where


import Data.String

import Kraken.TargetM


newtype Monitor = Monitor TargetName
  deriving (Eq, Ord, Show, IsString)

fromMonitor :: Monitor -> TargetName
fromMonitor (Monitor n) = n

-- | Type representing targets.
data Target = Target {
    name :: TargetName,
    dependencies :: [TargetName],
    monitor :: Maybe Monitor,
    run :: TargetM ()
  }
