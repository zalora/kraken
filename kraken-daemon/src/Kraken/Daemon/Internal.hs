{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kraken.Daemon.Internal where


import           Control.Monad.Trans.Either
import           Control.Monad.IO.Class        (liftIO)
import           Data.Aeson                    (ToJSON(..), FromJSON(..))
import           Data.Proxy
import           GHC.Generics
import           Network.Wai
import           Servant.API
import           Servant.Docs
import           Servant.Server

import           Kraken.ActionM
import           Kraken.Graph
import           Kraken.Store


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


-- * Target graphs

data WebNode = WebNode {
  monitor :: Maybe TargetName
 }
  deriving (Generic)

toWebNode :: Node -> WebNode
toWebNode node =
  WebNode (fmap nodeMonitorName (Kraken.Graph.nodeMonitor node))

instance FromJSON WebNode
instance ToJSON WebNode

instance FromJSON TargetName
instance ToJSON TargetName


newtype TargetGraph = TargetGraph (Graph TargetName WebNode)
  deriving (Generic)

instance FromJSON TargetGraph where
  parseJSON value = TargetGraph <$> fromListLenient <$> parseJSON value

instance ToJSON TargetGraph where
  toJSON (TargetGraph g) = toJSON $ toList g

instance ToSample TargetGraph where
  toSample = Just $ TargetGraph $ fromListLenient $
    ("a", WebNode (Just "a.monitor"), []) :
    ("b", WebNode Nothing, []) :
    ("c", WebNode (Just "c.monitor"), ["a", "b"]) :
    ("a.monitor", WebNode Nothing, []) :
    ("c.monitor", WebNode Nothing, []) :
    []


toTargetGraph :: Graph TargetName Node -> TargetGraph
toTargetGraph = TargetGraph . fmap toWebNode

-- | Create an application that simply serves the markdown documentation of
-- an API.
serveDocumentation :: HasDocs layout => Proxy layout -> Application
serveDocumentation api _ respond = respond $ responseLBS status200 [] md
    where
        md = cs . markdown $ docs api

-- | Run an application, logging port to systemd
runWarp :: Port -> Application -> IO ()
runWarp port application = do
  let settings =
        setPort port $
        setBeforeMainLoop (do
          let message = "listening on port " ++ show port
          hPutStrLn stderr message
          systemdNotify message) $
        defaultSettings
  runSettings settings application

systemdNotify :: String -> IO ()
systemdNotify message = do
  mExecutable <- findExecutable "systemd-notify"
  forM_ mExecutable $ \ executable -> do
    process <- spawnProcess executable $
      "--ready" :
      ("--status=" ++ message ++ "") :
      []
    _ <- waitForProcess process
    return ()
