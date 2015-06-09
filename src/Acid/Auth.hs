{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module defines the Auth binding
module Acid.Auth
  -- export all
  where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad.IO.Class
-- import qualified Control.Monad.State as MS
-- import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.AcidState
import qualified Data.Acid as A
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Session         (SessionManager, mkRNG)
import           Web.ClientSession            (getKey)
-- import           UHC.Util.Pretty
------------------------------------------------------------------------------
import           Application
import           Application.User
import           Acid.API
-- import           Utils.Monad
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Auth backend binding

instance IAuthBackend (AcidState AppAcid) where
    save acid user                 = A.update acid $ SaveAuthUser user
    lookupByUserId acid uid        = fmap (fmap (^. authUser)) $ A.query acid $ UserLookupByKeyAcid uid
    lookupByLogin  acid l          = fmap (fmap (^. authUser)) $ A.query acid $ UserLookupByNameAcid l
    lookupByRememberToken acid tok = fmap (fmap (^. authUser)) $ A.query acid $ UserLookupByRTokenAcid tok
    destroy acid user              = A.update acid $ UserDeleteAcid $ (authUser .~ user $ emptyUser)

------------------------------------------------------------------------------
-- | Init of Auth manager
initAcidAuthManager :: AuthSettings
                    -> AcidState AppAcid
                    -> SnapletLens b SessionManager
                    -> SnapletInit b (AuthManager b)
initAcidAuthManager settings acid lns =
    makeSnaplet
      "Blog-AcidStateAuthManager"
      "A snaplet providing user authentication using the Acid State backend for the Blog app"
      Nothing $ do
          rng  <- liftIO mkRNG
          key  <- liftIO $ getKey (asSiteKey settings)
          return AuthManager
                   { backend               = acid
                   , session               = lns
                   , activeUser            = Nothing
                   , minPasswdLen          = asMinPasswdLen settings
                   , rememberCookieName    = asRememberCookieName settings
                   , rememberPeriod        = asRememberPeriod settings
                   , siteKey               = key
                   , lockout               = asLockout settings
                   , randomNumberGenerator = rng
                   }
