{-# LANGUAGE DeriveGeneric, FlexibleInstances, OverloadedStrings,
             StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kraken.TargetGraph where


import           Control.Applicative
import           Data.Aeson           (ToJSON(..), FromJSON(..))
import           Data.Graph.Wrapper
import           GHC.Generics         (Generic)
import           Servant.Docs

import           Kraken.ActionM       (TargetName)
import           Kraken.Graph


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
