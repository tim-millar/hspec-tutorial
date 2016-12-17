{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module App (app, CurrentTime) where

import           GHC.Generics
import           Data.Aeson (ToJSON)
import           Network.Wai (Application)
import           Web.Scotty
import           Data.Time
import           Control.Monad.IO.Class (liftIO)

data Message = Message {
  body :: String
} deriving (Eq, Show, Generic)

instance ToJSON Message

data ServiceMessage = ServiceMessage {
    name    :: String
  , version :: String
} deriving (Eq, Show, Generic)

instance ToJSON ServiceMessage

data CurrentTime = CurrentTime {
  current_time :: String
} deriving (Eq, Show, Generic)

instance ToJSON CurrentTime

app :: IO UTCTime -> IO Application
app currentTime = scottyApp $ do
  get "/" $ do
    json (ServiceMessage "time-service" "0.1.0")
  get "/hello" $ do
    json (Message "Hello!")
  get "/current-time.json" $ do
    time <- liftIO currentTime
    json (CurrentTime (show time))
