{-# OPTIONS_GHC -fno-warn-orphans #-}
module Kraken.Orphans where

import           Control.Monad.Trans
import           Control.Monad.Trans.Either
import           System.Logging.Facade.Class

instance Logging m => Logging (EitherT e m) where
    consumeLogRecord = lift . consumeLogRecord
