module Test where

import Control.Monad.Reader (MonadReader (ask), ReaderT, asks)

k :: ReaderT Int IO String
k = do
  asks show