module Kraken.Util where


import           Control.Monad.IO.Class
import           Data.Char
import           System.IO


strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace


logMessage :: MonadIO m => String -> m ()
logMessage = liftIO . hPutStr stderr

logMessageLn :: MonadIO m => String -> m ()
logMessageLn = liftIO . hPutStrLn stderr
