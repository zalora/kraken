{-# LANGUAGE FlexibleInstances, OverloadedStrings, RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kraken.Daemon where


import           Network.Wai
import           Network.Wai.Handler.Warp.Run
import           Web.Scotty

import           Kraken.Store
import           Kraken.Web.TargetGraph


runDaemon :: Port -> Store -> IO ()
runDaemon port store = runWarp port =<< daemon store

daemon :: Store -> IO Application
daemon store = scottyApp $ do
  get "/targetGraph" $
    json (toTargetGraph (graph store))
