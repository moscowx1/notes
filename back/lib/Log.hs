module Log where

{-
type StartLogMessage = Text
type EndLogMessage = Text
type ErrorLogMessage = Text

withDebugLog ::
  (MonadLogger m) =>
  StartLogMessage ->
  m a ->
  EndLogMessage ->
  m a
withDebugLog s m e = do
  logDebugN s
  res <- m
  logDebugN e
  pure res

maybeWithLog ::
  (MonadLogger m) =>
  StartLogMessage ->
  m (Maybe a) ->
  ErrorLogMessage ->
  EndLogMessage ->
  m (Maybe a)
maybeWithLog startM action errM endM = do
  logDebugN startM
  action >>= \case
    Nothing -> logDebugN errM >> pure Nothing
    Just x -> logDebugN endM >> pure $ Just x

handleWithLog ::
  (MonadError ServerError m, MonadLogger m) =>
  Handle m ->
  Handle m
handleWithLog h =
  Handle
    { _generateSalt =
        withDebugLog
          "generating salt"
          (_generateSalt h)
          "salt generated"
    , _currentTime =
        withDebugLog
          "getting current time"
          (_currentTime h)
          "current time get"
    , _hashPassword =
        withDebugLog
          "hashing password time"
          (_hashPassword h)
          "password hashed"
    , _addToDb = undefined
    , _getUser = undefined
    , _setCookie = undefined
    }
-}