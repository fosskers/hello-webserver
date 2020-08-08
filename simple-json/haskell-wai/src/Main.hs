{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main where

import           Data.Aeson (FromJSON, ToJSON, decode', encode)
import           Data.Text (Text)
import           Data.Time.Calendar (addDays)
import           Data.Time.Clock (UTCTime(..))
import           GHC.Generics (Generic)
import           Network.HTTP.Types
import           Network.Wai
import qualified Network.Wai.Handler.Warp as W

data Colour = Red | Green | Blue
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

data User = User
  { name      :: !Text
  , age       :: !Word
  , profile   :: !Text
  , colour    :: !Colour
  , numbers   :: ![Int]
  , timestamp :: !UTCTime
  , missing   :: !(Maybe Bool) }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

app :: Application
app = routes
  where
    routes :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
    routes req resp = case pathInfo req of
      [] -> handleUser req >>= resp
      _  -> resp $ responseLBS status404 headers "Not Found"

    headers = [("Content-Type", "text/html")]

handleUser :: Request -> IO Response
handleUser req = case requestMethod req of
  "POST" -> do
    body <- strictRequestBody req
    pure $ case decode' body of
      Nothing   -> responseLBS status400 [] "Unable to parse JSON"
      Just user -> responseLBS status200 headers . encode $ tweakUser user
  _ -> pure $ responseLBS status405 [] ""
  where
    headers = [("Content-Type", "application/json")]

tweakUser :: User -> User
tweakUser u@(User _ a _ c n t@(UTCTime d _) _) =
  u { age = a + 1
    , colour = col
    , numbers = map (*3) n
    , timestamp = t { utctDay = addDays 1 d }
    , missing = Just True }
  where
    col = case c of
      Blue -> Red
      _    -> col

main :: IO ()
main = W.run 8080 app
