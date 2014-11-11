{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Kraken.Web where


import           Control.Exception
import           Control.Monad                  (when)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Trans.Either     hiding (left)
import           Data.Aeson                     (FromJSON(..), ToJSON(..))
import           Data.ByteString                (ByteString, hGetContents)
import           Data.Maybe
import           Data.Proxy
import           Data.String.Conversions
import           GHC.Generics                   (Generic)
import           Network.HTTP.Types
import           Network.Wai                    as Wai
import           Network.Wai.Handler.Warp.Run
import           Servant
import           Servant.Client
import           Servant.Docs
import           System.Exit
import           System.IO
import           System.Process                 (CreateProcess (..),
                                                 StdStream (..), createProcess,
                                                 proc, waitForProcess)
import           Kraken.Daemon                  hiding (Documentation)
import           Kraken.Dot
import           Kraken.TargetGraph

import           Kraken.Web.Config


run :: IO ()
run = do
  config <- loadConfig
  documentRoot <- getDocumentRoot
  runWarp (Kraken.Web.Config.port config) (application documentRoot (krakenUri config))

application :: FilePath -> BaseUrl -> Application
application documentRoot krakenUri =
  serve webApi $
  server documentRoot krakenUri

-- * API

type WebApi =
       "targetGraph.pdf" :> Raw
  :<|> "targetGraph.dot" :> Raw
  :<|> "target" :> Capture "target-name" String :> "monitor" :> "run"
                :> Get MonitorStatus
  :<|> "docs" :> Get Documentation
  :<|> Raw

webApi :: Proxy WebApi
webApi = Proxy

server :: FilePath -> BaseUrl -> Server WebApi
server documentRoot krakenUri =
       targetGraph krakenUri Pdf
  :<|> targetGraph krakenUri Dot
  :<|> monitorStatus krakenUri
  :<|> serveDocs webApi
  :<|> serveDirectory documentRoot

data FileFormat
  = Dot
  | Pdf

type ServantResponse a = EitherT (Int, String) IO a

serveDocs :: HasDocs layout => Proxy layout -> ServantResponse Documentation
serveDocs api = return . Documentation $ cs . markdown $ docs api

newtype Documentation = Documentation String
    deriving (Show, Eq, Generic)

instance ToJSON Documentation
instance FromJSON Documentation

instance ToSample Documentation where
  toSample = return $ Documentation "Some documentation"

-- * Monitors

-- | Check each daemon for the status of a particular monitor, until one
-- responds affirmatively.
monitorStatus :: BaseUrl -> String -> ServantResponse MonitorStatus
monitorStatus url name = do
    resp <- liftIO . runEitherT $ getMonitorStatus name url
    return $ case resp of
        Left err -> MonitorStatus (Err err) Nothing
        Right ms -> ms

-- * Graphs

targetGraph :: BaseUrl -> FileFormat -> Application
targetGraph krakenUri fileFormat request respond = do
  let prefixes =
        (\ xs -> if null xs then Nothing else Just xs) $
        map cs $
        catMaybes $
        map snd $
        filter ((== "prefix") . fst) $
        Wai.queryString request
  graphPartsWithErr <- runEitherT $ getTargetGraph krakenUri
  case graphPartsWithErr of
      Left err -> respond $ responseLBS status503
          [("Content-Type", "text/plain")] (cs err)
      Right graph -> do
        let dot = Kraken.Web.toDot prefixes graph
        case fileFormat of
          Pdf -> do
            (exitCode, pdf) <- readProcess "dot" ["-Tpdf"] dot
            when (exitCode /= ExitSuccess) $
              throwIO (ErrorCall ("dot exited with: " ++ show exitCode))
            respond $ responseLBS ok200 [("Content-Type", "application/pdf")] (cs pdf)
          Dot ->
            respond $ responseLBS ok200 [("Content-Type", "text/vnd.graphviz")] (cs dot)

toDot :: Maybe [String] -> TargetGraph -> String
toDot prefixes (TargetGraph g) = Kraken.Dot.toDot False prefixes True (fmap Kraken.Dot.fromWebNode g)


-- * daemon api

getTargetGraph :: BaseUrl -> EitherT String IO TargetGraph
getMonitorStatus :: String -> BaseUrl -> EitherT String IO MonitorStatus
(     getTargetGraph
 :<|> _
 :<|> getMonitorStatus)
    = client daemonApi


-- * utils

readProcess :: FilePath -> [String] -> String -> IO (ExitCode, ByteString)
readProcess path args input = do
  let process = (proc path args){
        std_in = CreatePipe,
        std_out = CreatePipe
       }
  (Just std_in, Just std_out, _, processHandle) <- createProcess process
  System.IO.hPutStr std_in input
  hClose std_in
  result <- Data.ByteString.hGetContents std_out
  exitCode <- waitForProcess processHandle
  return (exitCode, result)
