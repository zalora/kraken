{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Kraken.WebSpec where


import           Control.Concurrent
import           Control.Exception
import           Data.List
import           Data.String.Conversions
import           Network.Socket
import           Network.Wai.Handler.Warp as Warp
import           Network.Wai.Test
import           Servant                  hiding (get)
import           System.Process
import           Test.Hspec               hiding (pending)
import           Test.Hspec.Wai

import           Kraken.Daemon
import           Kraken.Graph
import           Kraken.Store
import           Kraken.Web               (application)


main :: IO ()
main = hspec spec

staticFilesDir :: FilePath
staticFilesDir = "static"

store :: Store
store = createStore $
  Target "target.1" [] (return ()) Nothing :
  Target "internalTarget.2" ["target.1"] (return ()) Nothing :
  Target "target.3" ["target.1", "internalTarget.2"] (return ()) Nothing :
  []

withKrakenDaemon :: (BaseUrl -> IO a) -> IO a
withKrakenDaemon action = bracket acquire free (\ (_, _, baseUrl) -> action baseUrl)
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
    let baseUrl = (BaseUrl Http "localhost" 80){baseUrlPort = krakenPort}
    return (thread, waitForKilled, baseUrl)
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
  beforeWith (\ baseUrl -> return (Kraken.Web.application staticFilesDir [baseUrl])) $ do
    describe "kraken-web" $ do
      it "returns the targetGraph as pdf" $ do
        response <- get "/targetGraph.pdf"
        return response `shouldRespondWith` 200{matchHeaders = [("Content-Type" <:> "application/pdf")]}
        liftIO $ do
          fileResult <- readProcess "file" ["-"] (cs $ simpleBody response)
          fileResult `shouldContain` "PDF"

      context "/targetGraph.dot" $ do
        it "returns the targetGraph as dot" $ do
          response <- get "/targetGraph.dot"
          return response `shouldRespondWith` 200{matchHeaders = [("Content-Type" <:> "text/vnd.graphviz")]}
          liftIO $ do
            let ws = words (cs (simpleBody response))
            ws `shouldContain` ["digraph"]
            ws `shouldContain` words "\"internalTarget.2\" -> \"target.1\""

        it "allows to set a prefix" $ do
          response <- get "/targetGraph.dot?prefix=target."
          liftIO $ cs (simpleBody response) `shouldSatisfy` (\ (body :: String) ->
            not ("2" `isInfixOf` body))

        it "allows to set multiple prefixes" $ do
          response <- get "/targetGraph.dot?prefix=target.&prefix=internalTarget."
          liftIO $ cs (simpleBody response) `shouldSatisfy` (\ (body :: String) ->
            ("2" `isInfixOf` body))

        it "returns a html page pointing to the targetGraph.pdf on /" $ do
          response <- get "/"
          liftIO $ (cs (simpleBody response)
            `shouldSatisfy` (("targetGraph.pdf" :: String) `isInfixOf`))
