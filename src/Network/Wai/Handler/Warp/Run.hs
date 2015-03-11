{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Handler.Warp.Run (
  Port,
  runWarp,
 ) where


import           Data.Foldable
import           Network.Wai
import           Network.Wai.Handler.Warp
import           System.Directory
import           System.Process

import qualified System.Logging.Facade as Log


runWarp :: Port -> Application -> IO ()
runWarp port application = do
  let settings =
        setPort port $
        setHost "127.0.0.1" $
        setBeforeMainLoop (do
          let message = "listening on port " ++ show port
          Log.info message
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
