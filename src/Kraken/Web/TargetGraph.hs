{-# LANGUAGE DeriveGeneric, FlexibleInstances, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kraken.Web.TargetGraph where


import           Control.Applicative
import           Data.Aeson
import           Data.Graph.Wrapper
import           GHC.Generics

import           Kraken.ActionM
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


newtype TargetGraph = TargetGraph (Graph TargetName (Integer, WebNode))
  deriving (Generic)

instance FromJSON TargetGraph where
  parseJSON value = TargetGraph <$> fromListLenient <$> parseJSON value

instance ToJSON TargetGraph where
  toJSON (TargetGraph g) = toJSON $ toList g

toTargetGraph :: Graph TargetName (Integer, Node) -> TargetGraph
toTargetGraph = undefined -- TargetGraph . fmap toWebNode
