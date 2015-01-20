{-# LANGUAGE OverloadedStrings, QuasiQuotes, ScopedTypeVariables #-}

module Kraken.DaemonSpec where


import           Control.Applicative
import           Control.Concurrent
import           Control.Exception
import           Data.Aeson
import           Data.Maybe
import           Data.String.Conversions
import           Network.HTTP.Client     hiding (port)
import           Network.HTTP.Types
import           Network.Wai.Test
import           System.Environment
import           System.Exit
import           System.IO
import           System.IO.Silently
import           Test.Hspec
import           Test.Hspec.Wai

import           Kraken.ActionM          (cancel)
import           Kraken.Daemon.Internal
import           Kraken.Graph
import           Kraken.Run              as Kraken
import           Kraken.Store


main :: IO ()
main = hspec spec

runAsMain :: Store -> IO ()
runAsMain = Kraken.runAsMain "test program"

runWithExitCode :: Store -> IO ExitCode
runWithExitCode store = Control.Exception.catch
  (Kraken.runAsMain "test program" store >> return ExitSuccess)
  (\ (e :: ExitCode) -> return e)

port :: Int
port = 8239

store :: Store
store = createStore $
  Target "foo" [] (return ()) Nothing :
  Target "bar" ["foo"] (return ()) Nothing :
  Target "baz" [] (return ()) (Just (Monitor "baz-monitor" [] $ const $ return ())) :
  Target "kaboom" [] (return ()) (Just $ Monitor "kaboom-monitor" [] $ const $ cancel "an error") :
  []

spec :: Spec
spec = do

  describe "daemon command" $ do
    it "returns the graph as a dot file" $
      withManager defaultManagerSettings $ \ manager -> do
        threadId <- forkIO $
          withArgs ["daemon", "--port", show port] $
          Kraken.runAsMain "test program" store
        threadDelay 100000

        url <- parseUrl ("http://localhost:" ++ show port ++ "/targetGraph")
        response :: String <- cs <$> responseBody <$> httpLbs url manager
        response `shouldContain` "foo"
        response `shouldContain` "bar"
        killThread threadId

    it "prints the port to stderr when started" $ do
      output <- hCapture_ [stderr] $ do
        threadId <- forkIO $
          withArgs ["daemon", "--port", show port] $
          Kraken.runAsMain "test program" store
        threadDelay 100000
        killThread threadId
      output `shouldContain` show port

    it "allows to use the global --config option" $ do
      output <- hCapture_ [stderr] $ do
        threadId <- forkIO $
          withArgs ["daemon", "--config", "kraken.conf.example", "--port", show port] $
          Kraken.runAsMain "test program" store
        threadDelay 100000
        killThread threadId
      output `shouldContain` show port

  describe "daemon application" $ appSpec

appSpec :: Spec
appSpec = with (return $ daemon store) $ do

  it "returns valid JSON under /targetGraph" $ do
    response <- get "/targetGraph"
    liftIO $ response `shouldSatisfy` isValidJson

  describe "target monitor endpoint" $ do

    it "allows monitors to be run" $ do
      response <- get "/target/baz/monitor/run"
      return response `shouldRespondWith` 200
      let mstatus = decode $ simpleBody response :: Maybe MonitorStatus
      liftIO $ mstatus `shouldSatisfy` isJust
      liftIO $ mstatus `shouldBe` (Just $ MonitorStatus OK $ Just "()")

    it "returns 404 for unknown targets" $ do
      get "/target/unknown/monitor/run" `shouldRespondWith` 404

    context "when the target does not have a monitor" $ do
      it "returns 400" $ do
        get "/target/bar/monitor/run" `shouldRespondWith` 400

    context "when the monitor fails" $ do
      it "returns an error in MonitorStatus" $ do
        response <- get "/target/kaboom/monitor/run"
        return response `shouldRespondWith` 200
        let Just (MonitorStatus (Err err) _) = decode $ simpleBody response
        liftIO $ err `shouldContain` "an error"

isValidJson :: SResponse -> Bool
isValidJson response =
  simpleStatus response == ok200 &&
  ("Content-Type", "application/json") `elem` simpleHeaders response &&
  isJust (decode' (simpleBody response) :: Maybe Value)
