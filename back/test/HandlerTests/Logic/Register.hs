{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HandlerTests.Logic.Register where

import Control.Monad.Except (ExceptT, MonadError (throwError), MonadTrans (lift), runExceptT)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.State (MonadState (get), StateT, evalStateT, modify')
import Data.Foldable (find)
import Data.Time (getCurrentTime)
import Database.Entities.User (Login, User, UserT (..))
import Dto.Auth (Credential (Credential))
import GHC.IO (unsafePerformIO)
import Handler.Logic.Register (register)
import Handler.Types.Register (CreateUserData (..), Handle (..), RegistrationError(..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

type Test = ExceptT RegistrationError (StateT [User] Identity)

evalTest :: Test a -> Either RegistrationError a
evalTest = runIdentity . (`evalStateT` []) . runExceptT

getUser :: Login -> Test (Maybe User)
getUser login =
  find
    (\u -> _userLogin u == login)
    <$> lift get

createUser :: CreateUserData -> User
createUser CreateUserData{..} =
  User
    { _userId = 0
    , _userCreatedAt = _createdAt
    , _userLogin = _login
    , _userPassword = _hashedPassword
    , _userSalt = _salt
    }

addUser :: CreateUserData -> Test (Maybe User)
addUser cud = do
  let user = createUser cud
  mu <- getUser (_userLogin user)
  case mu of
    Nothing -> do
      modify' (user :)
      pure $ Just user
    Just _ -> pure Nothing

createHandle :: Handle Test ()
createHandle =
  Handle
    { _generateSalt = pure ""
    , _currentTime = pure $ unsafePerformIO getCurrentTime
    , _hashPassword = const
    , _addUser = addUser
    , _setCookie = const (pure $ Just ())
    , _throw = throwError
    }

run ::
  Credential ->
  Either RegistrationError ()
run cred = evalTest $ register createHandle cred

registerTests :: TestTree
registerTests =
  testGroup
    "Register Logic tests"
    [ testCase "success register" $ do
        let cred = Credential "someLogin" "somePassword"
        run cred @?= pure ()
    , testCase "error invalid login" $ do
        let cred = Credential "" "somePassword"
        run cred @?= Left InvalidRequest
    , testCase "error invalid password" $ do
        let cred = Credential "someLogin" ""
        run cred @?= Left InvalidRequest
    , testCase "error duplicate login" $ do
      let cred = Credential "someLogin" "somePassword"
      let res = evalTest $ do
              let handle = createHandle
              register handle cred >> register handle cred
          
      res @?= Left LoginAlreadyTaken
    ]
