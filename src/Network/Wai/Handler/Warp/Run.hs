
module Network.Wai.Handler.Warp.Run (
  Port,
  runWarp,
 ) where


import           Network.Wai
import           Network.Wai.Handler.Warp
import           System.IO


runWarp :: Port -> Application -> IO ()
runWarp port application = do
  let settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings application
