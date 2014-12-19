{-|
Module      : Kraken.Daemon
Description : Tools for interacting with a kraken daemon
Copyright   : (c) Zalora SEA

@Kraken.Daemon@ provides the querying functions and tools needed to
interact with a kraken-daemon service. Anything that might be useful to a
client.

-}
module Kraken.Daemon (
   -- * API
     getTargetGraph
   , getDocs
   , getMonitorStatus

   -- * Types returned from or expected by the server
   , MonitorStatus(..)
   , Status(..)

   -- * Functions and types related to @TargetGraph@s
   , toTargetGraph
   , TargetGraph(..)
   , WebNode(..)
   , TargetName(..)
   ) where

import Kraken.Daemon.Internal
import Control.Monad.Trans.Either
import Servant.Client

getTargetGraph :: BaseUrl -> EitherT (Int, String) IO TargetGraph
getDocs :: BaseUrl -> EitherT (Int, String) IO String
getMonitorStatus :: BaseUrl -> EitherT (Int, String) IO MonitorStatus

getTargetGraph :<|> _ :<|> getDocs :<|> getMonitorStatus = client daemonApi
