{-# LANGUAGE FlexibleInstances, OverloadedStrings, RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kraken.Daemon where


import           Data.Aeson
import           Data.Graph.Wrapper
import           Data.Maybe
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.UrlMap
import           System.IO

import           Kraken.ActionM
import           Kraken.Graph
import           Kraken.Store


runDaemon :: Port -> Store -> IO ()
runDaemon port store = runSettings settings (daemon store)
  where
    settings =
      setPort port $
      setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
      defaultSettings

daemon :: Store -> Application
daemon store = mapUrls $
  mount "targetGraph" (jsonApplication (targetGraph store))

type JsonApplication =
  Request -> (Value -> IO ResponseReceived) -> IO ResponseReceived


jsonApplication :: JsonApplication -> Application
jsonApplication app request respond =
  app request (respond . responseLBS ok200 [("Content-Type", "application/json")] . encode)

targetGraph :: Store -> JsonApplication
targetGraph store _ respond = do
  respond (toJSON (graph store))

instance ToJSON (Graph TargetName Node) where
  toJSON graph = object ["targetGraph" .= map nodeToValue (toList graph)]

nodeToValue :: (TargetName, Node, [TargetName]) -> Value
nodeToValue (name, node, dependencies) = object $
  "name" .= show name :
  "dependencies" .= map show dependencies :
  "hasMonitor" .= isJust (monitor node) :
  []
