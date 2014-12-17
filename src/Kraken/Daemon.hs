{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kraken.Daemon where


import           Control.Monad.Trans.Either
import           Data.Aeson                    (ToJSON(..), FromJSON(..))
import           Data.Proxy
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp.Run
import           Servant.API
import           Servant.Docs
import           Servant.Server

import           Kraken.Store
import           Kraken.Web.TargetGraph
import           Kraken.Web.Utils


-- * API definition

type DaemonApi =
       "targetGraph" :> Get TargetGraph
  :<|> Get ()
  :<|> "docs" :> Raw
  :<|> "target" :> Capture "target-name" String :> "monitor" :> "status" :> Get MonitorStatus

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
  :<|> undefined

-- * API-related types

data MonitorStatus =
    MonitorStatus { status :: Status
                  , result :: Maybe String
                  } deriving (Show, Eq, Generic)

instance ToJSON MonitorStatus
instance FromJSON MonitorStatus

instance ToSample MonitorStatus where
  toSample = return $ MonitorStatus OK (Just "run output")

instance ToCapture (Capture "target-name" String) where
  toCapture _ = DocCapture "target-name" "name of the target"

data Status = OK
            | Err String
            deriving (Eq, Show, Generic)


instance ToJSON Status
instance FromJSON Status

