{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Kraken.Daemon.Internal where


import           Control.Monad.Trans.Either
import           Control.Monad.IO.Class        (liftIO)
import           Data.Aeson                    (ToJSON(..), FromJSON(..))
import           Data.Proxy                    (Proxy(..))
import           Data.String.Conversions       (cs)
import           GHC.Generics                  (Generic)
import           Network.Wai                   (Application)
import           Network.Wai.Handler.Warp.Run  (Port, runWarp)
import           Servant.API
import           Servant.Docs
import           Servant.Server

import           Kraken.ActionM
import           Kraken.Graph
import           Kraken.Store
import           Kraken.TargetGraph


-- * API definition

type DaemonApi =
       "targetGraph" :> Get TargetGraph
  :<|> "docs" :> Get Documentation
  :<|> "target" :> Capture "target-name" String :> "monitor" :> "run" :> Get MonitorStatus

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
      :<|> serveDocs daemonApi
      :<|> runTargetMonitor store

-- * Endpoint implementations

-- | Try to find and run the monitor for target @targetName@ in the store.
runTargetMonitor :: Store -> String -> EitherT (Int, String) IO MonitorStatus
runTargetMonitor store targetName = do
    result <- liftIO . runActionM $ lookupTarget store (TargetName targetName)
    case result of
        Right (_, node) -> case nodeMonitor node of
            Nothing -> left (400, "Target does not have associated monitor")
            Just NodeMonitor{..} -> do
                monResult <- liftIO . runActionM $ nodeMonitorAction Nothing
                case monResult of
                    Left err -> return $ MonitorStatus (Err $ show err) Nothing
                    Right a -> return $ MonitorStatus OK (Just $ show a)
        Left err -> left (404, show err)


serveDocs :: HasDocs layout => Proxy layout -> ServantResponse Documentation
serveDocs api = return . Documentation $ cs . markdown $ docs api

-- * API-related types

type ServantResponse a = EitherT (Int, String) IO a

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

newtype Documentation = Documentation String
    deriving (Show, Eq, Generic)

instance ToJSON Documentation
instance FromJSON Documentation

instance ToSample Documentation where
  toSample = return $ Documentation "Some documentation"
