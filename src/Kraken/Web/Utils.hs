module Kraken.Web.Utils where

import           Data.Proxy
import           Data.String.Conversions (cs)
import           Network.HTTP.Types (status200)
import           Network.Wai
import           Servant.Docs

-- | Create an application that simply serves the markdown documentation of
-- an API.
serveDocumentation :: HasDocs layout => Proxy layout -> Application
serveDocumentation api _ respond = respond $ responseLBS status200 [] md
    where
        md = cs . markdown $ docs api
