{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators      #-}

module Main where

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Proxy (Proxy(..))
import           Data.Text (Text)
import           Data.Time.Calendar (addDays)
import           Data.Time.Clock (UTCTime(..))
import           GHC.Generics (Generic)
import qualified Network.Wai.Handler.Warp as W
import           Servant.API
import           Servant.Server

type API = ReqBody '[JSON] User :> Post '[JSON] User

data Colour = Red | Green | Blue
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

data User = User
  { name      :: Text
  , age       :: Word
  , colour    :: Colour
  , numbers   :: [Int]
  , timestamp :: UTCTime
  , missing   :: Maybe Bool }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

server :: Server API
server = pure . tweakUser

app :: Application
app = serve (Proxy :: Proxy API) server

tweakUser :: User -> User
tweakUser u@(User _ a c n t@(UTCTime d _) _) =
  u { age = a + 1
    , colour = col
    , numbers = map (*2) n
    , timestamp = t { utctDay = addDays 1 d }
    , missing = Just True }
  where
    col = case c of
      Blue -> Red
      _    -> col

main :: IO ()
main = W.run 8080 app
