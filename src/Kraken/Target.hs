{-# LANGUAGE DeriveFunctor, GADTs, GeneralizedNewtypeDeriving, OverloadedStrings
             #-}

-- | Implements a simple target.

module Kraken.Target (
    TargetP(..),
    Target,
    TargetName(..),
    TargetM,
    Monitor(..),
    monitorName,
  ) where


import           Kraken.TargetM


-- | Type representing targets.
data TargetP dependencies = Target {
    name :: TargetName,
    dependencies :: dependencies,
    monitor :: Maybe (Monitor dependencies),
    run :: TargetM () ()
  }
    deriving (Functor)

type Target = TargetP [TargetName]

data Monitor dependencies where
    Monitor :: TargetName ->
               dependencies ->
               (Maybe monitorInput -> TargetM monitorInput ()) ->
               Monitor dependencies

instance Functor Monitor where
    fmap f (Monitor name deps action) = Monitor name (f deps) action

monitorName :: Monitor dependencies -> TargetName
monitorName (Monitor name _ _) = name
