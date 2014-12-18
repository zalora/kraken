{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kraken.Daemon where


import           Control.Monad.Trans.Either
import           Control.Monad.IO.Class        (liftIO)
import           Data.Aeson                    (ToJSON(..), FromJSON(..))
import           Data.Proxy
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp.Run
import           Servant.API
import           Servant.Docs
import           Servant.Server

import           Kraken.ActionM
import           Kraken.Graph
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
           return (toTargetGraph $ graph store)
      :<|> left (404, "not found")
      :<|> serveDocumentation daemonApi
      :<|> runTargetMonitor
  where
    runTargetMonitor :: String -> EitherT (Int, String) IO MonitorStatus
    runTargetMonitor targetName = do
        result <- liftIO . runActionM $ lookupTarget store (TargetName targetName)
        case result of
            Right (_, node) -> case nodeMonitor node of
                Nothing -> left (400, "Target does not have associated monitor")
                Just NodeMonitor{..} -> do
                    monResult <- liftIO . runActionM $ nodeMonitorAction Nothing
                    case monResult of
                        Left err -> return $ MonitorStatus (Err $ show err) Nothing
                        Right a -> return $ MonitorStatus OK (Just $ show a)
            Left err -> left (500, show err)



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

