{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Test where

import Crypto.Random.Entropy (getEntropy)
import Crypto.KDF.PBKDF2 (fastPBKDF2_SHA512, Parameters (..))
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Text.Encoding (encodeUtf8)

prm :: Parameters
prm = Parameters { iterCounts = 8192, outputLength = 64 }

t3 :: Text -> IO (ByteString, ByteString)
t3 pwd = let salt = getEntropy 32
             pwdB = encodeUtf8 pwd
  in (,) <$> salt <*> (fastPBKDF2_SHA512 prm pwdB <$> salt)

-- working
t :: Text -> IO Bool
t pwd = do
  ss <- getEntropy 32 :: IO ByteString
  let k1  = fastPBKDF2_SHA512 prm (encodeUtf8 pwd) ss :: ByteString
  let k2 =  fastPBKDF2_SHA512 prm (encodeUtf8 pwd) ss
  pure $ k1 == k2
-- not working
k :: Text -> IO Bool
k pwd = do
  (salt, hp1) <- t3 pwd
  let hp2 = fastPBKDF2_SHA512 prm (encodeUtf8 pwd) salt
  pure $ hp1 == hp2
