{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application
  -- export all
  where

------------------------------------------------------------------------------
-- import           Control.Monad.Trans
import           Control.Lens
import           Data.Typeable
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Session
import           Snap.Snaplet.AcidState
-- import           Data.Acid
import           Data.SafeCopy (base, deriveSafeCopy)
import           Config.SafeCopy
import           Application.User
import           Application.Blog
-- import qualified Utils as U

------------------------------------------------------------------------------
-- Acid state for App
data AppAcid = AppAcid
    { _users        :: Users
    }
    deriving (Typeable)

emptyAppAcid :: AppAcid
emptyAppAcid = AppAcid emptyUsers

makeLenses ''AppAcid

deriveSafeCopy safeCopyVersion 'base ''AppAcid

------------------------------------------------------------------------------
-- App global structure/data

data App = App
    { _heist :: Snaplet (Heist App)
    , _sess :: Snaplet SessionManager
    -- , _auth :: Snaplet (AuthManager App)
    , _authacid :: Snaplet (AuthManager App)
    , _acid :: Snaplet (Acid AppAcid)
    }

makeLenses ''App

instance HasHeist App where
  heistLens = subSnaplet heist

instance HasAcid App AppAcid where
  getAcidStore  = view $ acid . snapletValue

{-
class HasAuth b where  
  getAuthManager :: SnapletLens  (Snaplet b) (AuthManager b)

instance HasAuth App where
  getAuthManager = view $ authacid . snapletValue
-}

------------------------------------------------------------------------------
--

type AppHandler = Handler App App

