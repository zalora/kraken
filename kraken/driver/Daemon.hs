module Main where


import           Data.Foldable
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant.Server
import           System.Directory
import           System.IO
import           System.Process

import           Kraken.Daemon.Internal


runWarp :: Port -> Application -> IO ()
runWarp port application = do
  let settings =
        setPort port $
        setBeforeMainLoop (do
          let message = "listening on port " ++ show port
          hPutStrLn stderr message
          systemdNotify message) $
        defaultSettings
  runSettings settings application

systemdNotify :: String -> IO ()
systemdNotify message = do
  mExecutable <- findExecutable "systemd-notify"
  forM_ mExecutable $ \ executable -> do
    process <- spawnProcess executable $
      "--ready" :
      ("--status=" ++ message ++ "") :
      []
    _ <- waitForProcess process
    return ()
