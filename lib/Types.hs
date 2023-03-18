module Types (Password, Salt, Login, HashedPassword) where

import Data.ByteString (ByteString)
import Data.Text ( Text )

type Password = ByteString
type Salt = ByteString
type Login = Text
type HashedPassword = ByteString