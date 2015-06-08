{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

------------------------------------------------------------------------------
-- | This module defines the Acid API to the app functionality
module Acid.API
  -- export all
  where

------------------------------------------------------------------------------
import           Data.Maybe
import           Control.Lens
import           Snap.Snaplet.AcidState
import           Snap.Snaplet.Auth
------------------------------------------------------------------------------
import           Application
import           Application.User
import           Utils.Monad (liftST2MS, liftRT2MR)
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Acid API

-- | Add a user
userAddAcid :: UserName -> Maybe UserRToken -> Update AppAcid (Maybe UserKey)
userAddAcid nm mtok = liftST2MS $ zoom users $ userAdd nm mtok

-- | Set user
userSetByKeyAcid :: UserKey -> User -> Update AppAcid ()
userSetByKeyAcid k u = liftST2MS $ zoom users $ do
    userUpdateByKey k (const u)
    return ()

-- | Delete a user
userDeleteAcid :: User -> Update AppAcid ()
userDeleteAcid u = liftST2MS $ zoom users $ userDelete u

-- | Lookup a user by name
userLookupByNameAcid :: UserName -> Query AppAcid (Maybe User)
userLookupByNameAcid nm = liftRT2MR $ magnify users $ userLookupByName nm

-- | Lookup a user by key
userLookupByKeyAcid :: UserKey -> Query AppAcid (Maybe User)
userLookupByKeyAcid nm = liftRT2MR $ magnify users $ userLookupByKey nm

-- | Lookup a user  by token
userLookupByRTokenAcid :: UserRToken -> Query AppAcid (Maybe User)
userLookupByRTokenAcid nm = liftRT2MR $ magnify users $ userLookupByRToken nm

-- | Specific saving for Auth
saveAuthUser :: AuthUser -> Update AppAcid (Either AuthFailure AuthUser)
saveAuthUser user = liftST2MS $ zoom users $ do
    case userId user of
        Just k -> 
          fmap (maybe (Left DuplicateLogin) (Right . (^. authUser))) $ 
            userUpdateByKey k $ \u -> ((userName .~ (userLogin user)) . (authUser .~ user)) u
        Nothing -> do
          mk <- userAdd (userLogin user) (userRememberToken user)
          case mk of
            Just k -> do
              fmap (Right . (^. authUser) . fromJust) $ 
                userUpdateByKey k $ \u -> (authUser .~ (user {userId = userId $ u ^. authUser})) u
            Nothing -> return $ Left DuplicateLogin
{-
    mk <- userAdd (userLogin user) (userRememberToken user)
    case mk of
      Nothing -> case userId user of
        Just k -> 
          fmap (maybe (Left DuplicateLogin) (Right . (^. authUser))) $ 
            userUpdateByKey k $ \u -> (userName .~ (userLogin user)) u
        Nothing -> return $ Left DuplicateLogin
      Just k -> do
          fmap (Right . (^. authUser) . fromJust) $ 
            userUpdateByKey k $ \u -> (authUser .~ (user {userId = userId $ u ^. authUser})) u
-}

makeAcidic ''AppAcid
  [ 'userAddAcid
  , 'userSetByKeyAcid
  , 'userDeleteAcid
  
  , 'userLookupByNameAcid
  , 'userLookupByKeyAcid
  , 'userLookupByRTokenAcid
  
  , 'saveAuthUser
  ]

