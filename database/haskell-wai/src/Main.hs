{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main ( main ) where

import           Data.Aeson (ToJSON, encode)
import           Data.Maybe (listToMaybe)
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime(..))
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.Ok
import           GHC.Generics (Generic)
import           Network.HTTP.Types
import           Network.Wai
import qualified Network.Wai.Handler.Warp as W

data Colour = Red | Green | Blue
  deriving stock (Generic)
  deriving anyclass (ToJSON)

-- An efficient Int-based encoding, but unfortuantely boilerplate.
instance FromField Colour where
  fromField f = case fieldData f of
    SQLInteger 0 -> Ok Red
    SQLInteger 1 -> Ok Green
    SQLInteger 2 -> Ok Blue
    _            -> returnError ConversionFailed f "Value out of range"

data User = User
  { name      :: !Text
  , age       :: !Word
  , profile   :: !Text
  , colour    :: !Colour
  , timestamp :: !UTCTime
  , missing   :: !(Maybe Bool) }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

-- You'd think this could be auto-derived.
instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field

app :: Connection -> Application
app c = routes
  where
    routes :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
    routes req resp = case (requestMethod req, pathInfo req) of
      ("GET", [n]) -> userLookup c n >>= resp
      _            -> resp $ responseLBS status404 headers "Not Found"

    headers = [("Content-Type", "text/html")]

userLookup :: Connection -> Text -> IO Response
userLookup c n = userLookup' c n >>= \case
  Nothing -> pure $ responseLBS status400 [] "Invalid user name"
  Just u  -> pure . responseLBS status200 [("Content-Type", "application/json")] $ encode u

userLookup' :: Connection -> Text -> IO (Maybe User)
userLookup' c n =
  listToMaybe <$> query c "select * from test where name = ? limit 1" (Only n)

main :: IO ()
main = open "../test.db" >>= W.run 8080 . app
