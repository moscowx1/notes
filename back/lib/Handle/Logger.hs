{-# LANGUAGE OverloadedStrings #-}

module Handle.Logger (Handle(..), mkLogger) where

import Control.Monad.Cont (MonadIO (liftIO))
import Data.Time (getCurrentTime)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data Handle m = Handle
  { _logDebug :: Text -> m ()
  , _logInfo :: Text -> m ()
  , _logError :: Text -> m ()
  }

data LogLvl = Debug | Info | Error
  deriving (Show)

mkMsg :: LogLvl -> Text -> IO Text
mkMsg lvl msg = do
  cur <- getCurrentTime
  pure $ mconcat
    [ T.pack (show cur)
    , " "
    , T.pack (show lvl)
    , " "
    , msg
    , "\n"
    ]

-- TODO: need to optimize or use library
-- api works x2 slower
log' :: FilePath -> LogLvl -> Text -> IO ()
log' fn lvl msg = do
  msg' <- mkMsg lvl msg
  _ <- liftIO $ TIO.appendFile fn msg'
  liftIO $ TIO.putStr msg'

mkLogger :: (MonadIO m) => FilePath -> Handle m
mkLogger fn = Handle 
  { _logDebug = liftIO . log' fn Debug
  , _logInfo = liftIO . log' fn Info
  , _logError = liftIO . log' fn Error
  }
