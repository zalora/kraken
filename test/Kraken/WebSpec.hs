{-# LANGUAGE OverloadedStrings #-}

module Kraken.WebSpec where


import           Control.Concurrent
import           Control.Exception
import           Data.Maybe
import           Data.String.Conversions
import           Network.URI
import           Network.Wai.Handler.Warp as Warp
import           Network.Wai.Test
import           System.Process
import           Test.Hspec
import           Test.Hspec.Wai

import           Kraken.Daemon
import           Kraken.Graph
import           Kraken.Store
import           Kraken.Web               (application)


main :: IO ()
main = hspec spec

store :: Store
store = createStore $
  Target "target.a" [] (return ()) Nothing :
  Target "target.b" ["target.a"] (return ()) Nothing :
  []

krakenPort :: Port
krakenPort = 8287

config :: [URI]
config =
  (fromMaybe (error "error parsing test uri") (parseURI ("http://localhost:" ++ show krakenPort))) :
  []

withKrakenDaemon :: IO a -> IO a
withKrakenDaemon action = bracket acquire free (const action)
 where
  acquire = do
    mvar <- newEmptyMVar
    thread <- forkIO $ do
      let settings =
            setPort krakenPort $
            setBeforeMainLoop (putMVar mvar ())
            defaultSettings
      Warp.runSettings settings (daemon store)
    takeMVar mvar
    return thread
  free = killThread


spec :: Spec
spec =
  around_ withKrakenDaemon $
  with (Kraken.Web.application config) $ do
    describe "kraken-web" $ do
      it "returns the targetGraph as dot" $ do
        response <- get "/targetGraph.dot"
        return response `shouldRespondWith` 200{matchHeaders = [("Content-Type" <:> "text/vnd.graphviz")]}
        liftIO $ do
          let ws = words (cs (simpleBody response))
          ws `shouldContain` ["digraph"]
          ws `shouldContain` words "\"target.b\" -> \"target.a\""

      it "returns the targetGraph as pdf" $ do
        response <- get "/targetGraph.pdf"
        return response `shouldRespondWith` 200{matchHeaders = [("Content-Type" <:> "application/pdf")]}
        liftIO $ do
          fileResult <- readProcess "file" ["-"] (cs $ simpleBody response)
          fileResult `shouldContain` "PDF"
