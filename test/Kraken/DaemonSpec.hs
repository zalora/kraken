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
  []

spec :: Spec
spec = do

  describe "daemon command" $ do
    it "returns the graph as a dot file" $
      withManager defaultManagerSettings $ \ manager -> do
        let thread =
              withArgs ["daemon", "--port", show port] $
              Kraken.runAsMain "test program" store
        withForkedThread thread $ do
          threadDelay 100000

          url <- parseUrl ("http://localhost:" ++ show port ++ "/targetGraph")
          response :: String <- cs <$> responseBody <$> httpLbs url manager
          response `shouldContain` "foo"
          response `shouldContain` "bar"

    it "prints the port to stderr when started" $ do
      output <- hCapture_ [stderr] $ do
        let thread =
              withArgs ["daemon", "--port", show port] $
              Kraken.runAsMain "test program" store
        withForkedThread thread $
          threadDelay 100000
      output `shouldContain` show port

    it "allows to use the global --config option" $ do
      output <- hCapture_ [stderr] $ do
        let thread =
              withArgs ["daemon", "--config", "kraken.conf.example", "--port", show port] $
              Kraken.runAsMain "test program" store
        withForkedThread thread $
          threadDelay 100000
      output `shouldContain` show port

  describe "daemon application" $ appSpec

appSpec :: Spec
appSpec = with (return $ daemon store) $ do
    it "returns valid JSON under /targetGraph" $ do
      response <- get "/targetGraph"
      liftIO $ response `shouldSatisfy` isValidJson

isValidJson :: SResponse -> Bool
isValidJson response =
  simpleStatus response == ok200 &&
  ("Content-Type", "application/json") `elem` simpleHeaders response &&
  isJust (decode' (simpleBody response) :: Maybe Value)

-- * thread helpers

fork :: IO () -> IO (MVar (), ThreadId)
fork action = do
  mvar <- newMVar ()
  thread <- forkIO $ modifyMVar_ mvar $ \ () -> action
  return (mvar, thread)

kill :: (MVar (), ThreadId) -> IO ()
kill (mvar, thread) = do
  killThread thread
  modifyMVar_ mvar $ \ () -> return ()

withForkedThread :: IO () -> IO () -> IO ()
withForkedThread forked action = do
  thread <- fork forked
  action `finally` kill thread
