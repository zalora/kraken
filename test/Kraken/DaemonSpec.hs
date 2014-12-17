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

import           Kraken.Daemon
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

    it "returns a 404 for /" $ do
      get "/" `shouldRespondWith` 404{matchBody = Just "not found"}

    it "allows monitors to be run" $ do
      get "/targets/baz/monitor/status" `shouldRespondWith` 200

isValidJson :: SResponse -> Bool
isValidJson response =
  simpleStatus response == ok200 &&
  ("Content-Type", "application/json") `elem` simpleHeaders response &&
  isJust (decode' (simpleBody response) :: Maybe Value)
