{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module AuthTest where

import Control.Monad.Except (
  ExceptT,
  MonadError (
    throwError
  ),
  MonadTrans (lift),
  runExceptT,
 )
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.State (MonadState (get), StateT, evalStateT, modify, modify')
import Data.Foldable (find)
import Data.Text.Encoding (decodeUtf8)
import Data.Time (getCurrentTime)
import DataAccess.Data (Login, User (..))
import Dto.Auth (Credential (..))
import GHC.IO (unsafePerformIO)
import Handle.Logger (Handle (..))
import qualified Handle.Logger as Logger
import Logic.Auth (AuthError, Handle (..))
import qualified Logic.Auth as Auth
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

type Test = ExceptT AuthError (StateT [User] Identity)

evalTestM :: [User] -> Test a -> Either AuthError a
evalTestM st = runIdentity . (`evalStateT` st) . runExceptT

getUser :: Login -> Test (Maybe User)
getUser login =
  find
    (\u -> u.userLogin == login)
    <$> lift get

addUser :: User -> Test (Maybe User)
addUser user = do
  mu <- getUser user.userLogin
  case mu of
    Nothing -> do
      modify' (user :)
      pure $ Just user
    Just _ -> pure Nothing

logger :: Logger.Handle Test
logger =
  Logger.Handle
    { _logInfo = const $ pure ()
    , _logDebug = const $ pure ()
    , _logError = const $ pure ()
    }

dummyHandle :: Auth.Handle Test ()
dummyHandle =
  Auth.Handle
    { _generateSalt = pure ""
    , _currentTime = pure $ unsafePerformIO getCurrentTime
    , _hashPassword = const
    , _addToDb = addUser
    , _getUser = getUser
    , _authentificate = const (pure $ Just ())
    , _throw = throwError
    , _logger = logger
    }

tests :: TestTree
tests =
  testGroup
    "Testing login"
    [ testCase "Success login" $ do
        let userLogin = "master"
            password = "qwerty"
        let res = evalTestM [] $ do
              userSalt <- _generateSalt dummyHandle
              let userPassword = _hashPassword dummyHandle password userSalt
              userCreatedAt <- _currentTime dummyHandle
              _ <- modify (User{..} :)
              let cred = Credential userLogin (decodeUtf8 userPassword)
              Auth.signIn dummyHandle cred

        res @?= pure ()
    ]