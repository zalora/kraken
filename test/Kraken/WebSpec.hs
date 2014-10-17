{-# LANGUAGE OverloadedStrings #-}

module Kraken.WebSpec where


import           Control.Concurrent
import           Control.Exception
import           Data.Maybe
import           Data.String.Conversions
import           Network.Socket
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

config :: Port -> [URI]
config krakenPort =
  (fromMaybe (error "error parsing test uri") (parseURI ("http://localhost:" ++ show krakenPort))) :
  []

withKrakenDaemon :: (Port -> IO a) -> IO a
withKrakenDaemon action = bracket acquire free (\ (_, _, port) -> action port)
 where
  acquire = do
    (notifyStart, waitForStart) <- lvar
    (notifyKilled, waitForKilled) <- lvar
    thread <- forkIO $ (do
      (krakenPort, socket) <- openTestSocket
      let settings =
            setPort krakenPort $ -- for consistency, shouldn't be used (it's set in the socket)
            setBeforeMainLoop (notifyStart krakenPort)
            defaultSettings
      Warp.runSettingsSocket settings socket (daemon store))
            `finally` notifyKilled ()
    krakenPort <- waitForStart
    return (thread, waitForKilled, krakenPort)
  free (thread, waitForKilled, _) = do
    killThread thread
    waitForKilled

  lvar :: IO (a -> IO (), IO a)
  lvar = do
    mvar <- newEmptyMVar
    let put = putMVar mvar
        wait = readMVar mvar
    return (put, wait)

openTestSocket :: IO (Port, Socket)
openTestSocket = do
  s <- socket AF_INET Stream defaultProtocol
  localhost <- inet_addr "127.0.0.1"
  bind s (SockAddrInet aNY_PORT localhost)
  listen s 1
  port <- socketPort s
  return (fromIntegral port, s)


spec :: Spec
spec =
  around withKrakenDaemon $
  beforeWith (\ krakenPort -> Kraken.Web.application (config krakenPort)) $ do
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
