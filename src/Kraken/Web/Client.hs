module Kraken.Web.Client ( mkJqueryBindings ) where

import Servant
import Servant.JQuery

import Kraken.Web

-- | Generate jquery bindings for kraken-web and place them in the given
-- @FilePath@.
mkJqueryBindings :: FilePath -> IO ()
mkJqueryBindings lib = do
   let pdfTargetGraph :<|> dotTargetGraph :<|> runMonitor :<|> docs :<|> _ = jquery webApi
   let intro = "// This file was auto-generated - changes will be overwritten!"
   writeFile lib $ intro ++ concatMap generateJS [ pdfTargetGraph "GET"
                                                 , dotTargetGraph "GET"
                                                 , runMonitor
                                                 , docs
                                                 ]

