{-# LANGUAGE OverloadedStrings #-}

module Cors (corsMiddleware) where

import Data.CaseInsensitive (mk)
import Data.Foldable (find)
import Network.Wai (Middleware, Request (requestHeaders))
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), Origin, cors)

getOrigin :: Request -> Maybe Origin
getOrigin req =
  snd <$> find (\(name, _) -> name == mk "Origin") (requestHeaders req)

corsMiddleware :: Middleware
corsMiddleware = cors $ \req -> do
  Just $ myPolicy (getOrigin req)
 where
  myPolicy origin =
    CorsResourcePolicy
      { corsOrigins = origin >>= \o -> pure ([o], True)
      , corsMethods = ["GET", "PUT", "POST", "OPTIONS"]
      , corsRequestHeaders =
          [ "Cache-Control"
          , "Content-Language"
          , "Content-Type"
          , "Content-Encoding"
          , "Expires"
          , "Authorization"
          , "Accept"
          ]
      , corsExposedHeaders = Nothing
      , corsMaxAge = Nothing
      , corsVaryOrigin = True
      , corsRequireOrigin = False
      , corsIgnoreFailures = False
      }
