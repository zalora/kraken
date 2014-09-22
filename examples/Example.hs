{-# LANGUAGE OverloadedStrings #-}

module Example where


import           Control.Monad
import           Control.Monad.IO.Class
import           Data.String
import           System.Directory

import           Kraken


-- | Exposes a command line interface to query the store
-- and run Targets from it. You can run this module as
-- 'Main' and play around with the command line interface.
-- For example you could try this (more than once):
--
-- $ runhaskell -isrc examples/example.hs run oof failingTarget
main :: IO ()
main = runAsMain "kraken example" $ createStore targets


-- | List of all the Targets that are being put in the store.
targets :: [Target]
targets =
  foo ++
  oof ++
  failingTarget ++
  []

-- | Creates the file foo with some contents in it.
foo :: [Target]
foo =
  Target "foo" [] (liftIO $ writeFile "foo" "hello") (Just $ fileMonitor "foo") :
  []

-- | Creates the file oof that contains the contents of foo reversed.
-- It depends on Target foo.
oof :: [Target]
oof =
  Target "oof" ["foo"]
    -- File 'foo' should exist, because Target 'oof' depends on Target 'foo'.
    (liftIO (readFile "foo" >>= writeFile "oof" . reverse)) (Just $ fileMonitor "oof") :
  []

-- | This is a Target that fails to create the file it's meant to create.
-- This will be caught by the supplied monitor when trying to execute the Target.
failingTarget :: [Target]
failingTarget =
  Target "failingTarget" [] (return ()) (Just $ fileMonitor "does_not_exist") :
  []

-- | Checks whether the given file exists.
fileMonitor :: FilePath -> Monitor
fileMonitor file = Monitor (fromString (file ++ ".monitor")) [] $ \ memoizedExists -> do
  exists <- memoizeMonitorInput memoizedExists $ liftIO $ doesFileExist file
  when (not exists) $ do
    -- If the file does not exist 'abort' is used to indicate
    -- that the monitor failed. The associated Target should
    -- be run to create the file.
    outOfDate ("file " ++ file ++ " does not exist") True
