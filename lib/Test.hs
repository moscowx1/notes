{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Test where

import Data.ByteString (ByteString)
import Crypto.Random.Entropy (getEntropy)
import Crypto.KDF.PBKDF2 (fastPBKDF2_SHA512, Parameters (Parameters, iterCounts, outputLength))
import Data.ByteArray (ByteArray(..), ByteArrayAccess(..))
import Data.Functor((<&>))
import Data.ByteString (pack)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

salt' :: ByteArray byteArray => Int -> IO byteArray
salt' = getEntropy

prm :: Parameters
prm = Parameters { iterCounts = 4000, outputLength = 50 }

t2 :: (ByteArrayAccess password, ByteArray out, ByteArray bt) => password -> IO bt -> IO out
t2 pwd s = s <&> fastPBKDF2_SHA512 prm pwd

m :: (forall o. ByteArray o) => IO out
m = t2 pwd' (salt' 30)
  where
    pwd' :: ByteString
    pwd' = "hi"

--saltPwd :: Text -> IO (Text, Text)
--saltPwd pwd = do
--  let pwd' = encodeUtf8 pwd
--  s <- salt' 1
--  pure ("a", "b")
