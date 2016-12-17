{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module AppSpec (spec) where

import           Test.Hspec hiding (pending)
import           Test.Hspec.Wai
import           Test.Hspec.Wai.QuickCheck
import           Test.Hspec.Wai.JSON
import           Data.ByteString
import           Data.String
import           Data.Time
import           Data.Time.Format
import           Data.Maybe

import           App (app)

spec :: Spec
spec = with (app $ return time) $ do
  describe "GET /" $ do
    it "responds with HTTP status 200" $ do
      get "/" `shouldRespondWith` 200

    it "responds with its name and version number" $ do
      get "/" `shouldRespondWith` [json|{name: "time-service", version: "0.1.0"}|]

    context "when given an invalid request path" $ do
      it "responds with HTTP status 404" $ do
        get "/invalid/path" `shouldRespondWith` 404

  describe "GET /hello" $ do
    it "responds with a hello message" $ do
      get "/hello" `shouldRespondWith` [json|{body: "Hello!"}|]

  describe "GET /current-time.json" $ do
    it "responds with the currrent time" $ do
      get "/current-time.json" `shouldRespondWith` [json|{current_time: "2016-12-16 14:26:28 UTC"}|]

  context "when given a *arbitrary* invalid request path" $ do
    it "responds with HTTP status 404" $ do
      property $ do
        \h t -> let { str :: ByteString; str = h `cons` t }
               in  get ("/" `append` str) `shouldRespondWith` 404

time :: UTCTime
time = fromJust $ maybeTime

maybeTime :: Maybe UTCTime
maybeTime = parseTimeM True defaultTimeLocale dateFormat date

dateFormat :: String
dateFormat = "%m/%d/%Y %l:%M:%S %p"

date :: String
date = "12/16/2016 14:26:28 PM"

instance Arbitrary ByteString where
    arbitrary = fmap pack arbitrary
