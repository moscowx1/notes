{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module AuthTest (loginTests, registerTests) where

import Control.Monad.Except (
  ExceptT,
  MonadError (throwError),
  MonadTrans (lift),
  runExceptT,
 )
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.State (MonadState (get), StateT, evalStateT, modify, modify')
import Data.Foldable (find)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (getCurrentTime)
import DataAccess.Data (Login, User (..))
import Dto.Auth (Credential (..))
import GHC.IO (unsafePerformIO)
import Handle.Logger (Handle (..))
import qualified Handle.Logger as Logger
import Logic.Auth (AuthError (..), Handle (..))
import qualified Logic.Auth as Auth
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

type Test = ExceptT AuthError (StateT [User] Identity)

evalTestM :: Test a -> Either AuthError a
evalTestM = runIdentity . (`evalStateT` []) . runExceptT

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

handle :: Auth.Handle Test ()
handle =
  Auth.Handle
    { _generateSalt = pure ""
    , _currentTime = pure $ unsafePerformIO getCurrentTime
    , _hashPassword = const
    , _addToDb = addUser
    , _getUser = getUser
    , _authenticate = const (pure $ Just ())
    , _throw = throwError
    , _logger = logger
    }

addUserM :: Auth.Handle Test a -> Credential -> Test ()
addUserM Auth.Handle{..} cred = do
  userSalt <- _generateSalt
  let userPassword = _hashPassword (encodeUtf8 $ password cred) userSalt
      userLogin = login cred
  userCreatedAt <- _currentTime
  modify (User{..} :)

type ExistUserCreds = Credential

loginTest ::
  ExistUserCreds ->
  Credential ->
  Either AuthError ()
loginTest exCred cred = evalTestM $ do
  _ <- addUserM handle exCred
  Auth.signIn handle cred

loginTests :: TestTree
loginTests =
  testGroup
    "Login tests"
    [ testCase "success login" $ do
        let cred = Credential "master" "qewqret"
        loginTest cred cred @?= pure ()
    , testCase "wrong login" $ do
        let cred = Credential "master" "qewqret"
        let cred2 = cred{login = "Asazxcv"}
        loginTest cred cred2 @?= Left WrongLogin
    , testCase "wrong password" $ do
        let cred = Credential "master" "qewqret"
        let cred2 = cred{password = "asdfasdf"}
        loginTest cred cred2 @?= Left WrongPassword
    ]

regTest ::
  Credential ->
  Either AuthError ()
regTest cred = evalTestM $ Auth.register handle cred

registerTests :: TestTree
registerTests =
  testGroup
    "Register tests"
    [ testCase "success register" $ do
        let cred = Credential "master" "qewqret"
        regTest cred @?= pure ()
    , testCase "invalid login" $ do
        let cred = Credential "" "qewqret"
        regTest cred @?= Left InvalidLogin
    , testCase "invalid password" $ do
        let cred = Credential "qwerasdf" ""
        regTest cred @?= Left InvalidPassword
    , testCase "login already exist" $ do
        let cred = Credential "qwerasdf" "asdfasdf"
        let res = evalTestM $ do
              addUserM handle cred
              Auth.register handle cred
        res @?= Left LoginAlreadyTaken
    ]