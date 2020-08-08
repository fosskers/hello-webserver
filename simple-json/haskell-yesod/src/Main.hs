{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module Main where

import Data.Aeson.Types (Result(..))
import Data.Text (Text)
import Data.Time.Calendar (addDays)
import Data.Time.Clock (UTCTime(..))
import GHC.Generics (Generic)
import Network.HTTP.Types.Status (status200, status400)
import Yesod

---

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

data HelloWorld = HelloWorld

mkYesod "HelloWorld" [parseRoutes|
/ HomeR POST
|]

instance Yesod HelloWorld

postHomeR :: Handler RepJson
postHomeR = parseCheckJsonBody >>= \case
  Error _ -> sendResponseStatus status400 ("Unparsable body" :: Text)
  Success u -> sendStatusJSON status200 $ tweakUser u

main :: IO ()
main = warp 8080 HelloWorld
