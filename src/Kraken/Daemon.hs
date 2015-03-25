{-# LANGUAGE FlexibleInstances, OverloadedStrings, RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kraken.Daemon where


import           Data.Aeson
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp.Run
import           Network.Wai.UrlMap

import           Kraken.Store
import           Kraken.Web.TargetGraph


runDaemon :: Port -> Store -> IO ()
runDaemon port store = runWarp port (daemon store)

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
  respond (toJSON (toTargetGraph $ graphWithoutPriorities store))
