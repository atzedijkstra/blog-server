{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application
  -- export all
  where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad.State
import           Data.Typeable
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Session
import           Snap.Snaplet.AcidState
import           Data.SafeCopy (base, deriveSafeCopy)
import qualified UHC.Util.RelMap as Rel
------------------------------------------------------------------------------
import           Utils.SafeCopy()
import           Config.SafeCopy
import           Application.KeyValueWithId
import           Application.User
import           Application.Blog
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Acid state for App
data AppAcid = AppAcid
    { _users        :: !Users
    , _blogs        :: !Blogs
    , _user2blog    :: Rel.Rel UserKey BlogKey
    }
    deriving (Typeable)

emptyAppAcid :: AppAcid
emptyAppAcid = AppAcid emptyUsers emptyBlogs Rel.empty

makeLenses ''AppAcid

deriveSafeCopy safeCopyVersion 'base ''AppAcid

------------------------------------------------------------------------------
-- App global structure/data

data App = App
    { _heist :: Snaplet (Heist App)
    , _sess :: Snaplet SessionManager
    , _authacid :: Snaplet (AuthManager App)
    , _acid :: Snaplet (Acid AppAcid)
    }

makeLenses ''App

instance HasHeist App where
  heistLens = subSnaplet heist

instance HasAcid App AppAcid where
  getAcidStore  = view $ acid . snapletValue

------------------------------------------------------------------------------
-- Functionality of acid part of app

-- | Associate blog and user
blogAttachToUser :: (Monad m, MonadState AppAcid m) => BlogKey -> UserKey -> m ()
blogAttachToUser b u = user2blog %= Rel.insert u b

-- | Lookup user for blog
user2blogLookupUserByBlog :: Blog -> AppAcid -> Maybe User
user2blogLookupUserByBlog blog acid = do
    uid <- Rel.lookupRng (blog ^. blogId) (acid ^. user2blog)
    kviLookupById' uid (acid ^. users)


------------------------------------------------------------------------------
--

type AppHandler = Handler App App

