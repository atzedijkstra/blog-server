{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module defines per user maintained state
module Application.User
  -- all is exported
  where

------------------------------------------------------------------------------
import           Data.Typeable
import           Control.Lens
import           Control.Monad.State
import           Control.Monad.Reader
import           Snap.Snaplet.Auth
import           Data.SafeCopy
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H
import           Data.Hashable (Hashable)
import           Data.Aeson (Value)
import           Data.Scientific(Scientific)
------------------------------------------------------------------------------
-- import           Marks.HtmlUI.FormTypes
import           Config.SafeCopy
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- AuthUser specific stuff

deriving instance Typeable UserId
deriving instance Typeable AuthUser

instance (SafeCopy a, SafeCopy b, Eq a, Hashable a) =>
    SafeCopy (H.HashMap a b) where
      getCopy = contain $ fmap H.fromList safeGet
      putCopy = contain . safePut . H.toList

deriveSafeCopy safeCopyVersion 'base ''UserId
deriveSafeCopy safeCopyVersion 'base ''AuthUser
deriveSafeCopy safeCopyVersion 'base ''Password
deriveSafeCopy safeCopyVersion 'base ''Role
deriveSafeCopy safeCopyVersion 'base ''AuthFailure

deriveSafeCopy safeCopyVersion 'base ''Value

deriveSafeCopy safeCopyVersion 'base ''Scientific

------------------------------------------------------------------------------
-- Per user maintained persistent state
type UserName = T.Text

data User = User
    { _userName     :: UserName
    , _authUser     :: AuthUser
    }
    deriving (Typeable)

emptyUser :: User
emptyUser = User "" defAuthUser

makeLenses ''User

deriveSafeCopy safeCopyVersion 'base ''User

------------------------------------------------------------------------------
-- Mapping from user to state

-- | Identification for user
type UserKey = UserId
type UserKeyGen = Int

type UserRToken = T.Text

-- | Relation between Name, Key and User
data Users = Users
    { _usersNextKey        :: !UserKeyGen
    , _usersName2KeyMp     :: Map.Map UserName UserKey
    , _usersToken2KeyMp    :: Map.Map UserRToken UserKey
    , _usersKey2UserMp     :: Map.Map UserKey User
    }
    deriving (Typeable)

makeLenses ''Users

emptyUsers :: Users
emptyUsers = Users 0 Map.empty Map.empty Map.empty

deriveSafeCopy safeCopyVersion 'base ''Users

------------------------------------------------------------------------------
-- Functionality on User and Users

-- | Add a user, state monadically
userAdd :: (Monad m, MonadState Users m) => UserName -> Maybe UserRToken -> m (Maybe UserKey)
userAdd nm mtok = do
    r <- use usersName2KeyMp
    case Map.lookup nm r of
      Just _ -> return Nothing
      _ -> do
        newKey <- usersNextKey %%= \k -> (UserId $ T.pack $ show k, k+1)
        usersName2KeyMp %= Map.insert nm newKey
        usersToken2KeyMp %= maybe id (\tok -> Map.insert tok newKey) mtok
        usersKey2UserMp %=
          (Map.insert newKey $
            ((userName .~ nm) . (authUser %~ \u -> u {userId = Just newKey, userLogin = nm, userRememberToken = mtok}))
              emptyUser)
        return $ Just newKey

-- | Delete a user
userDelete :: (Monad m, MonadState Users m) => User -> m ()
userDelete u = do
    let nm = userLogin $ u ^. authUser
    usersName2KeyMp  %= Map.delete nm
    usersToken2KeyMp %= (maybe id Map.delete $ userRememberToken $ u ^. authUser)
    usersKey2UserMp  %= (maybe id Map.delete $ userId $ u ^. authUser)

-- | Update user
userUpdateByKey :: (Monad m, MonadState Users m) => UserKey -> (User -> User) -> m (Maybe User)
userUpdateByKey k upd = do
    usersKey2UserMp %= Map.update (Just. upd) k
    m <- use usersKey2UserMp
    return $ Map.lookup k m 

-- | Lookup user by key
userLookupByKey :: (Functor m, Monad m, MonadReader Users m) => UserKey -> m (Maybe User)
userLookupByKey k = fmap (Map.lookup k) $ view usersKey2UserMp

-- | Lookup user by name
userLookupByName :: (Functor m, Monad m, MonadReader Users m) => UserName -> m (Maybe User)
userLookupByName nm = do
    mk <- fmap (Map.lookup nm) $ view usersName2KeyMp
    maybe (return Nothing) userLookupByKey mk

-- | Lookup user by rtoken
userLookupByRToken :: (Functor m, Monad m, MonadReader Users m) => UserRToken -> m (Maybe User)
userLookupByRToken rt = do
    mk <- fmap (Map.lookup rt) $ view usersToken2KeyMp
    maybe (return Nothing) userLookupByKey mk
