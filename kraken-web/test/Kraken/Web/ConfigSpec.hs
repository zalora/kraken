
module Kraken.Web.ConfigSpec where


import           Control.Applicative
import           Control.DeepSeq
import           Control.Exception
import           Data.Aeson
import           Data.String.Conversions
import           Network.Wai.Handler.Warp
import           Servant.Client             (parseBaseUrl)
import           System.Directory
import           System.Environment
import           System.IO.Temp
import           Test.Hspec

import           Kraken.Web.Config


main :: IO ()
main = hspec spec

mkConfig :: Port -> [String] -> Config
mkConfig port uris = Config {
  port = port,
  krakenUris = map (either error id . parseBaseUrl) uris
 }

spec :: Spec
spec = do
  describe "loadConfig" $ do
    it "loads from kraken-web.conf by default" $ do
      withSystemTempDirectory "kraken-test" $ \ dir -> do
        withCurrentDirectory dir $ do
          let config = mkConfig 9832 $
                "http://foo.com/" :
                "http://bla.sg/" :
                []
          writeFile "kraken-web.conf" $ cs $ encode config
          withArgs [] loadConfig `shouldReturn` config

    it "loads from a file given by --config" $ do
      withSystemTempDirectory "kraken-test" $ \ dir -> do
        withCurrentDirectory dir $ do
          let config = mkConfig 8439 $
                "http://bar.com/" :
                "http://baz.sg/" :
                []
          writeFile "something.file" $ cs $ encode config
          withArgs (words "--config something.file") loadConfig
            `shouldReturn` config

  describe "kraken-web.conf.example" $ do
    it "can be parsed as a configuration file" $ do
      config <- withArgs (words "--config kraken-web.conf.example")
        loadConfig
      deepseq (encode config) (return ())

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory dir action = bracket
  (getCurrentDirectory <* setCurrentDirectory dir)
  setCurrentDirectory
  (const action)
