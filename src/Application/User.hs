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
import           Data.SafeCopy
import qualified Data.Map as Map
import qualified Data.Text as T
------------------------------------------------------------------------------
-- import           Marks.HtmlUI.FormTypes
import           Config.SafeCopy
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Per user maintained persistent state
type UserName = T.Text

data User = User
    { _userName     :: UserName
    }
    deriving (Typeable)

emptyUser :: User
emptyUser = User ""

makeLenses ''User

deriveSafeCopy safeCopyVersion 'base ''User

------------------------------------------------------------------------------
-- Mapping from user to state

-- | Identification for user
type UserKey = Int

-- | Relation between Name, Key and User
data Users = Users
    { _usersNextKey        :: !Int
    , _usersName2KeyMp     :: Map.Map UserName UserKey
    , _usersKey2UserMp     :: Map.Map UserKey User
    }
    deriving (Typeable)

makeLenses ''Users

emptyUsers :: Users
emptyUsers = Users 0 Map.empty Map.empty

deriveSafeCopy safeCopyVersion 'base ''Users

------------------------------------------------------------------------------
-- Functionality on User and Users

-- | Add a user, state monadically
userAddM :: (Monad m, MonadState Users m) => UserName -> m (Maybe UserKey)
userAddM nm = do
    r <- use usersName2KeyMp
    case Map.lookup nm r of
      Just _ -> return Nothing
      _ -> do
        newKey <- usersNextKey %%= \k -> (k, k+1)
        usersName2KeyMp %= Map.insert nm newKey
        usersKey2UserMp %= (Map.insert newKey $ (userName .~ nm) emptyUser)
        return $ Just newKey

