{-# LANGUAGE OverloadedStrings #-}

module Config (readConnectInfo) where

import Data.Ini (lookupValue, Ini)
import Data.Text (unpack)
import Database.Beam.Postgres (ConnectInfo (..))
import Text.Read (readMaybe)

type ParseErrMessage = String
read' :: (Read a) => ParseErrMessage -> String -> Either String a
read' err value = maybe (Left err) Right (readMaybe value)

readConnectInfo :: Ini -> Either String ConnectInfo
readConnectInfo ini = do
  db <- lookup' "database"
  port <- lookup' "port" >>= read' "error parse port"
  pwd <- lookup' "password"
  host <- lookup' "host"
  user <- lookup' "user"
  pure $ ConnectInfo 
    { connectDatabase = db
    , connectHost = host
    , connectPassword =pwd
    , connectPort = port
    , connectUser =user 
    }
  where
    lookup' key = unpack <$> lookupValue "database" key ini
