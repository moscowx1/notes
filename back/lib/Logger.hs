module Logger where

import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger (LoggingT (runLoggingT), NoLoggingT (runNoLoggingT), defaultOutput)
import Data.Functor (($>))
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.IO (
  BufferMode (LineBuffering),
  IOMode (AppendMode),
  hSetBuffering,
  stdout,
  withFile,
 )

runAppLoggingT :: MonadIO m => FilePath -> LoggingT m a -> m a
runAppLoggingT fp m = do
  fh <- liftIO $
    withFile fp AppendMode $
      \h -> hSetBuffering h LineBuffering $> h

  _ <- liftIO $ hDuplicateTo fh stdout
  runLoggingT m $ defaultOutput fh