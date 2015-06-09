{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module defines per user maintained state
module Application.User
  -- all is exported
  where

------------------------------------------------------------------------------
import           Data.Maybe
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
import           UHC.Util.Pretty
------------------------------------------------------------------------------
import           Config.SafeCopy
import           Utils.Monad()
import           Application.KeyValueWithId
------------------------------------------------------------------------------
-- import           Utils.Debug
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

instance PP UserId where
  pp = pp . unUid

instance PP Password where
  pp = pp . show

instance PP AuthUser where
  pp (AuthUser {userLogin=l, userId=i, userRememberToken=tk, userPassword=pw, userFailedLoginCount=flog, userLoginCount=log}) =
    l >|< ppParens i >#<
    text "pw=" >|< show pw >#<
    text "rtok=" >|< tk >#<
    text "nlogin=" >|< log >#<
    text "nfaillogin=" >|< flog >#<
    empty

------------------------------------------------------------------------------
-- Per user maintained persistent state
type UserName = T.Text

data User = User
    { _userName     :: !UserName
    , _authUser     :: !AuthUser
    }
    deriving (Typeable)

emptyUser :: User
emptyUser = User "" defAuthUser

makeLenses ''User

deriveSafeCopy safeCopyVersion 'base ''User

instance Show User where
  show _ = "User"

instance PP User where
  pp (User {_userName=n, _authUser=u}) = n >|< text ":" >#< u

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

instance Show Users where
  show _ = "Users"

instance PP Users where
  pp (Users {_usersNextKey=nk, _usersName2KeyMp=n2k, _usersToken2KeyMp=t2k, _usersKey2UserMp=k2u}) =
    text "Users" >#< ppParens nk >-< indent 2
      ( Map.toList k2u >-< Map.toList n2k >-< Map.toList t2k
      )

instance KeyValueWithId Users where
  type Id Users     = UserKey
  type Key Users    = UserName
  type Val Users    = User
  kviKey2IdLens     = usersName2KeyMp
  kviId2ValLens     = usersKey2UserMp
  kviNextId         = usersNextKey %%= \k -> (UserId $ T.pack $ show k, k+1)
  kviKeyIdOfVal u   = return (Just $ userLogin $ u ^. authUser, fromJust $ userId $ u ^. authUser)
  kviPreDeleteVal u = usersToken2KeyMp %= (maybe id Map.delete $ userRememberToken $ u ^. authUser)
  kviPostInsertVal k u= usersToken2KeyMp %= (maybe id (\tok -> Map.insert tok k) $ userRememberToken $ u ^. authUser)

------------------------------------------------------------------------------
-- Functionality on User and Users

-- | Add a user, state monadically
userAdd :: (Monad m, MonadState Users m) => UserName -> Maybe UserRToken -> m (Maybe UserKey)
userAdd = kviAdd $ \nm newKey mtok -> ((userName .~ nm) . (authUser %~ \u -> u {userId = Just newKey, userLogin = nm, userRememberToken = mtok})) emptyUser

-- | Delete a user
userDelete :: (Monad m, MonadState Users m) => User -> m ()
userDelete = kviDelete

-- | Update user, return updated User
userUpdateByKey :: (Monad m, MonadState Users m) => UserKey -> (User -> User) -> m (Maybe User)
userUpdateByKey = kviUpdateById

-- | Lookup user by key
userLookupByKey :: (Functor m, Monad m, MonadReader Users m) => UserKey -> m (Maybe User)
userLookupByKey = kviLookupById

-- | Lookup user by name
userLookupByName :: (Functor m, Monad m, MonadReader Users m) => UserName -> m (Maybe User)
userLookupByName = kviLookupByKey

-- | Lookup user by rtoken
userLookupByRToken :: (Functor m, Monad m, MonadReader Users m) => UserRToken -> m (Maybe User)
userLookupByRToken rt = do
    mk <- fmap (Map.lookup rt) $ view usersToken2KeyMp
    maybe (return Nothing) userLookupByKey mk
