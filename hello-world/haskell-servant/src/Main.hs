{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Proxy (Proxy(..))
import           Data.Text (Text)
import qualified Network.Wai.Handler.Warp as W
import           Servant.API
import           Servant.Server

type API = Get '[PlainText] Text

server :: Server API
server = pure "Hello, World!"

app :: Application
app = serve (Proxy :: Proxy API) server

main :: IO ()
main = W.run 8080 app
