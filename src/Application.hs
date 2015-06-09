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
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Session
import           Snap.Snaplet.AcidState
import           Data.SafeCopy (base, deriveSafeCopy)
import           Config.SafeCopy
import           Application.User
import           Application.Blog
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Acid state for App
data AppAcid = AppAcid
    { _users        :: !Users
    , _blogs        :: !Blogs
    , _blogsOfUser  :: Map.Map UserKey (Set.Set BlogKey)
    }
    deriving (Typeable)

emptyAppAcid :: AppAcid
emptyAppAcid = AppAcid emptyUsers emptyBlogs Map.empty

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
-- Relation between blog and user
blogAttachToUser :: (Monad m, MonadState AppAcid m) => BlogKey -> UserKey -> m ()
blogAttachToUser b u = blogsOfUser %= Map.insertWith Set.union u (Set.singleton b)

------------------------------------------------------------------------------
--

type AppHandler = Handler App App

