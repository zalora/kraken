{-# LANGUAGE DataKinds, DeriveGeneric, ScopedTypeVariables, TypeOperators #-}

module RestExample where


import           Control.Monad.Trans.Either
import           Data.Aeson
import           Data.Proxy
import           GHC.Generics
import           Network.URI
import           Network.Wai
import           Network.Wai.TypedRest


-- defining custom types

data Person = Person {
  firstName :: String,
  lastName :: String
 }
  deriving (Eq, Show, Generic)

instance FromJSON Person
instance ToJSON Person

data Animal = Animal {
  name :: String,
  species :: String
 }
  deriving (Eq, Show, Generic)

instance FromJSON Animal
instance ToJSON Animal


-- defining the api

type Api =
  "foo" :> Get Person :<|>
  "bar" :> Get Animal :<|>
  Get ()

api :: Proxy Api
api = Proxy


-- implementing a server

application :: Application
application = serve api $
  (Proxy :: Proxy "foo") :> servePerson :<|>
  (Proxy :: Proxy "bar") :> serveAnimal :<|>
  (return ())

servePerson :: EitherT (Int, String) IO Person
servePerson = return $ Person "Paula" "Schmad"

serveAnimal :: EitherT (Int, String) IO Animal
serveAnimal = return $ Animal "Jerry" "mouse"


-- using the API client-side

getFoo :: URI -> EitherT String IO Person
getBar :: URI -> EitherT String IO Animal

((Proxy :: Proxy "foo") :> getFoo :<|>
 (Proxy :: Proxy "bar") :> getBar :<|>
 _) = client api
