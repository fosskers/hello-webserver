{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Network.HTTP.Types
import           Network.Wai
import qualified Network.Wai.Handler.Warp as W

app :: Application
app = routes
  where
    routes req resp = case pathInfo req of
      [] -> resp $ responseLBS status200 headers "Hello, World!"
      _  -> resp $ responseLBS status404 headers "Not Found"

    headers = [("Content-Type", "text/html")]

main :: IO ()
main = W.run 8080 app
