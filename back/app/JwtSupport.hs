{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module JwtSupport where

import GHC.Generics (Generic (Rep))
import Servant (
  GServantProduct,
  HasServer (ServerT),
  NamedRoutes,
  ToServant,
  ToServantApi,
  toServant,
 )
import Servant.Auth.Server.Internal.AddSetCookie (
  AddSetCookieApi,
  AddSetCookies (..),
  Nat (S),
 )
import Servant.Server.Generic (AsServerT)


-- need for jwt in NamedRoutes
-- fixed in servant auth server 0.4.8.0
-- delete after updating package

type instance AddSetCookieApi (NamedRoutes api) = AddSetCookieApi (ToServantApi api)

instance
  {-# OVERLAPS #-}
  ( AddSetCookies ('S n) (ServerT (ToServantApi api) m) cookiedApi
  , Generic (api (AsServerT m))
  , GServantProduct (Rep (api (AsServerT m)))
  , ToServant api (AsServerT m) ~ ServerT (ToServantApi api) m
  ) =>
  AddSetCookies ('S n) (api (AsServerT m)) cookiedApi
  where
  addSetCookies cookies = addSetCookies cookies . toServant
