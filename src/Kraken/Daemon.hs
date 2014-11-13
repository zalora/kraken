{-# LANGUAGE DataKinds, TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kraken.Daemon where


import           Control.Monad.Trans.Either
import           Data.Proxy
import           Network.Wai
import           Network.Wai.Handler.Warp.Run
import           Servant

import           Kraken.Store
import           Kraken.Web.TargetGraph


-- * API definition

type DaemonApi =
       "targetGraph" :> Get TargetGraph
  :<|> Get ()
  :<|> "docs" :> Raw

daemonApi :: Proxy DaemonApi
daemonApi = Proxy


-- * server implementation

runDaemon :: Port -> Store -> IO ()
runDaemon port store = runWarp port (daemon store)

daemon :: Store -> Application
daemon store = serve daemonApi (server store)

server :: Store -> Server DaemonApi
server store =
       (return $ toTargetGraph $ graph store)
  :<|> left (404, "not found")
  :<|> serveDocumentation daemonApi
