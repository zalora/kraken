{-# LANGUAGE OverloadedStrings #-}

module Kraken.Web where


import           Control.Applicative
import           Control.Exception
import           Control.Monad                (when)
import           Data.Aeson
import           Data.ByteString              (ByteString, hGetContents)
import           Data.Maybe
import           Data.String.Conversions
import           Network.HTTP.Client          as Client
import           Network.HTTP.Types
import           Network.URI
import           Network.Wai                  as Wai
import           Network.Wai.Handler.Warp.Run
import           Network.Wai.UrlMap
import           System.Exit
import           System.IO
import           System.Process               (CreateProcess (..),
                                               StdStream (..), createProcess,
                                               proc, waitForProcess)

import           Kraken.Dot
import           Kraken.Web.Config
import           Kraken.Web.TargetGraph


run :: IO ()
run = do
  config <- loadConfig
  runWarp (Kraken.Web.Config.port config) =<< application (krakenUri config)

application :: URI -> IO Application
application krakenUri = Client.withManager Client.defaultManagerSettings $ \ manager ->
  return $ mapUrls $
    mount "targetGraph.pdf" (targetGraph krakenUri manager Pdf) <|>
    mount "targetGraph.dot" (targetGraph krakenUri manager Dot)

data FileFormat
  = Dot
  | Pdf

targetGraph :: URI -> Manager -> FileFormat -> Application
targetGraph krakenUri manager fileFormat request respond = do
  let prefixes =
        (\ xs -> if null xs then Nothing else Just xs) $
        map cs $
        catMaybes $
        map snd $
        filter ((== "prefix") . fst) $
        Wai.queryString request
  graphParts <- getValue manager krakenUri
  let dot = Kraken.Web.toDot prefixes graphParts
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

toDot :: Maybe [String] -> TargetGraph -> String
toDot prefixes (TargetGraph g) = Kraken.Dot.toDot False prefixes True (fmap (uncurry Kraken.Dot.fromWebNode) g)


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
