{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

------------------------------------------------------------------------------
-- | This module defines the Acid API to the app functionality
module Acid.API
  -- export all
  where

------------------------------------------------------------------------------
import           Data.Maybe
import qualified UHC.Util.RelMap as Rel
import           Control.Lens
import           Control.Monad.Reader
import           Snap.Snaplet.AcidState
import           Snap.Snaplet.Auth
------------------------------------------------------------------------------
import           Application
import           Application.User
import           Application.Blog
import           Utils.Monad
------------------------------------------------------------------------------
-- import           UHC.Util.Pretty
-- import           Utils.Debug
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Acid API for users

-- | All users
usersAcid :: Query AppAcid Users
usersAcid = asks _users

-- | Add a user
userAddAcid :: UserName -> Maybe UserRToken -> Update AppAcid (Maybe UserKey)
userAddAcid nm mtok = liftST2MS $ zoom users $ userAdd nm mtok

-- | Set user
userSetByKeyAcid :: UserKey -> User -> Update AppAcid ()
userSetByKeyAcid k u = liftST2MS $ zoom users $ do
    voidM $ userUpdateByKey k $ const u

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
      Just k -> do
        u <- userUpdateByKey k $ \u -> ((userName .~ (userLogin user)) . (authUser .~ user)) u
        seq u $ return $ (maybe (Left DuplicateLogin) (Right . (^. authUser))) u
      Nothing -> do
        mk <- userAdd (userLogin user) (userRememberToken user)
        case mk of
          Just k -> do
            u <- userUpdateByKey k $ \u -> (authUser .~ (user {userId = userId $ u ^. authUser})) u
            seq u $ return $ (Right . (^. authUser) . fromJust) u
          Nothing -> return $ Left DuplicateLogin

------------------------------------------------------------------------------
-- Acid API for blogs

-- | All blogs
blogsAcid :: Query AppAcid Blogs
blogsAcid = asks _blogs

-- | Set blog
blogSetByKeyAcid :: BlogKey -> Blog -> Update AppAcid ()
blogSetByKeyAcid k u = liftST2MS $ zoom blogs $ do
    voidM $ blogUpdateByKey k $ const u

-- | Add a blog for a user
blogAddForUserAcid :: UserKey -> Blog -> Update AppAcid (Maybe BlogKey)
blogAddForUserAcid userKey blog = liftST2MS $ do
    mkey <- zoom blogs $ blogAdd (blog ^. blogTitle)
    case mkey of
      Just key -> do
        zoom blogs $ blogUpdateByKey key $ \b -> (blogId .~ (b ^. blogId)) blog
        blogAttachToUser key userKey
        return mkey
      _ -> return Nothing

-- | Lookup a blog by key
blogLookupByKeyAcid :: BlogKey -> Query AppAcid (Maybe Blog)
blogLookupByKeyAcid nm = liftRT2MR $ magnify blogs $ blogLookupByKey nm

------------------------------------------------------------------------------
-- Acid API for ownership

-- | All ownership
blogsOfUserAcid :: Query AppAcid [(UserKey, [BlogKey])]
blogsOfUserAcid = fmap Rel.toDomList $ asks _user2blog

------------------------------------------------------------------------------
-- Acid API select/queries combining various subcomponents

-- | Extract/select blogs and their users
blogsSelectByMbUserAcid :: Maybe UserKey -> Query AppAcid [(Blog,User)]
blogsSelectByMbUserAcid mbForUser = do
    acid <- ask
    fmap concat $ 
      forM (blogsToList $ acid ^. blogs) $ \blog -> do
        let mbuser = user2blogLookupUserByBlog blog acid
        if (maybe True id $ do {u1 <- mbForUser; u2 <- mbuser; return $ u1 == userKey u2})
          then return [(blog, fromJust mbuser)]
          else return []

------------------------------------------------------------------------------
-- Acid API overall

-- | All
acidAll :: Query AppAcid AppAcid
acidAll = ask

------------------------------------------------------------------------------
-- Acidification
makeAcidic ''AppAcid
  [ 
  -- user
    'usersAcid
  
  , 'userAddAcid
  , 'userSetByKeyAcid
  , 'userDeleteAcid
  
  , 'userLookupByNameAcid
  , 'userLookupByKeyAcid
  , 'userLookupByRTokenAcid
  
  , 'saveAuthUser
  
  -- blog
  , 'blogsAcid
  
  , 'blogAddForUserAcid
  , 'blogSetByKeyAcid
  
  , 'blogLookupByKeyAcid
  
  -- ownership
  , 'blogsOfUserAcid
  
  -- combi
  , 'blogsSelectByMbUserAcid
  
  -- all
  , 'acidAll
  ]

