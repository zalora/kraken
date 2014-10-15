{-# LANGUAGE OverloadedStrings #-}

module Kraken.Web where


import           Control.Applicative
import           Control.Exception
import           Control.Monad            (when)
import           Data.Aeson
import           Data.ByteString          (ByteString, hGetContents)
import           Data.Graph.Wrapper
import           Data.String.Conversions
import           Data.Traversable         (forM)
import           Network.HTTP.Client      as Client hiding (port)
import           Network.HTTP.Types
import           Network.URI
import           Network.Wai              as Wai
import qualified Network.Wai.Handler.Warp as Warp
import           Network.Wai.UrlMap
import           System.Exit
import           System.IO
import           System.Process           (CreateProcess (..), StdStream (..),
                                           createProcess, proc, waitForProcess)

import           Kraken.Dot
import           Kraken.Web.Config
import           Kraken.Web.TargetGraph


run :: IO ()
run = do
  config <- loadConfig
  let settings =
        Warp.setPort (port config) $
        Warp.setBeforeMainLoop
          (hPutStrLn stderr ("listening on port " ++ show (port config))) $
        Warp.defaultSettings
  Warp.runSettings settings =<< application (krakenUris config)

application :: [URI] -> IO Application
application krakenUris = Client.withManager Client.defaultManagerSettings $ \ manager ->
  return $ mapUrls $
    mount "targetGraph.pdf" (targetGraph krakenUris manager Pdf) <|>
    mount "targetGraph.dot" (targetGraph krakenUris manager Dot)

data FileFormat
  = Dot
  | Pdf

targetGraph :: [URI] -> Manager -> FileFormat -> Application
targetGraph krakenUris manager fileFormat _request respond = do
  graphParts <- forM krakenUris $ \ uri ->
    getValue manager uri
  let dot = Kraken.Web.toDot $ mergeGraphs graphParts
  case fileFormat of
    Pdf -> do
      (exitCode, pdf) <- readProcess "dot" ["-Tpdf"] dot
      when (exitCode /= ExitSuccess) $
        throwIO (ErrorCall ("dot exited with: " ++ show exitCode))
      respond $ responseLBS ok200 [("Content-Type", "application/pdf")] (cs pdf)
    Dot ->
      respond $ responseLBS ok200 [("Content-Type", "text/vnd.graphviz")] (cs dot)

-- | Retrieves a value from a kraken daemon.
getValue :: FromJSON result => Manager -> URI -> IO result
getValue manager uri = do
  innerRequest <- Client.parseUrl (show (nullURI{uriPath = "/targetGraph"} `relativeTo` uri))
  innerResponse <- Client.httpLbs innerRequest manager
  when (Client.responseStatus innerResponse /= ok200) $
    throwIO $ ErrorCall ("kraken daemon returned: " ++ show (Client.responseStatus innerResponse))
  maybe (throwIO (ErrorCall "kraken daemon returned invalid json")) return $
    decode' (Client.responseBody innerResponse)

mergeGraphs :: [TargetGraph] -> TargetGraph
mergeGraphs graphs = TargetGraph $ fromListLenient $
  concatMap (\ (TargetGraph g) -> toList g) graphs

toDot :: TargetGraph -> String
toDot (TargetGraph g) = Kraken.Dot.toDot False Nothing True (fmap Kraken.Dot.fromWebNode g)


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
