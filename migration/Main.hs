{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Config (Config(..))
import Data.Aeson(eitherDecodeFileStrict)
import Data.Functor ((<&>))
import Utils (throwLeft)

main :: IO ()
main = do
  print "started migration"
  Config { _dbConnect }<- eitherDecodeFileStrict "config.json" <&> throwLeft
  print "database migrated"

