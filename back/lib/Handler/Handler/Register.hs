module Handler.Handler.Register where

import Data.Pool (Pool, withResource)
import Database.Beam.Postgres (Connection)
import Dto.Auth (Credential)
import Handler.DataAccess.Register (addUser)
import Handler.Logic.Register (Handle (..))
import qualified Handler.Logic.Register as L

register ::
  Pool Connection ->
  Credential ->
  IO ()
register pool _ = withResource pool $ \connection ->
  let handle =
        Handle
          { _addUser = addUser
          , _connection = connection
          , _throw = undefined
          , _currentTime = undefined
          , _hashPassword = undefined
          , _generateSalt = undefined
          }
   in L.register handle undefined
