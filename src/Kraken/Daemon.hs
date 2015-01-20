-- | Since this module is meant to be imported by client libraries (in
-- particular, kraken-web), it re-exports only the relevant subset of
-- 'Kraken.Daemon.Internal' to allay fears of tight coupling.
module Kraken.Daemon (
   -- * API
   DaemonApi
   , daemonApi
   -- * API types
   , MonitorStatus(..)
   , Status(..)
   , Documentation(..)
   ) where

import Kraken.Daemon.Internal
