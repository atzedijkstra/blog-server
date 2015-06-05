{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module defines the Auth binding
module Acid.Auth
  -- export all
  where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad.IO.Class
-- import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.AcidState
import qualified Data.Acid as A
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Session         (SessionManager, mkRNG)
import           Web.ClientSession            (getKey)
------------------------------------------------------------------------------
import           Application
import           Application.User
import           Acid.API
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Auth backend binding

instance IAuthBackend (AcidState AppAcid) where
    save a u                    = A.update a $ SaveAuthUser u
    lookupByUserId a uid        = fmap (fmap (^. authUser)) $ A.query a $ UserLookupByKeyAcid uid
    lookupByLogin  a l          = fmap (fmap (^. authUser)) $ A.query a $ UserLookupByNameAcid l
    lookupByRememberToken a tok = fmap (fmap (^. authUser)) $ A.query a $ UserLookupByRTokenAcid tok
    destroy a u                 = A.update a $ UserDeleteAcid $ (authUser .~ u $ emptyUser)

------------------------------------------------------------------------------
-- | Init of Auth manager
initAcidAuthManager :: AuthSettings
                    -> AcidState AppAcid
                    -> SnapletLens b SessionManager
                    -> SnapletInit b (AuthManager b)
initAcidAuthManager s a lns =
    makeSnaplet
      "Blog-AcidStateAuthManager"
      "A snaplet providing user authentication using the Acid State backend for the Blog app"
      Nothing $ do
          -- removeResourceLockOnUnload
          rng  <- liftIO mkRNG
          key  <- liftIO $ getKey (asSiteKey s)
          -- dir  <- getSnapletFilePath
          -- acid <- liftIO $ openLocalStateFrom dir emptyUS
          return AuthManager
                   { backend               = a
                   , session               = lns
                   , activeUser            = Nothing
                   , minPasswdLen          = asMinPasswdLen s
                   , rememberCookieName    = asRememberCookieName s
                   , rememberPeriod        = asRememberPeriod s
                   , siteKey               = key
                   , lockout               = asLockout s
                   , randomNumberGenerator = rng
                   }
