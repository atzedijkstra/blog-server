{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Data.Maybe
import           Control.Lens
import           Control.Applicative
import           Data.ByteString.Char8 as B (pack, ByteString)
import           Data.Monoid
import qualified Data.Text as T
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
-- import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Snaplet.AcidState
import           Snap.Util.FileServe
import           Heist
import qualified Heist.Interpreted as I
------------------------------------------------------------------------------
import qualified Text.Digestive.Snap as DS
import           Text.Digestive.Heist
------------------------------------------------------------------------------
import           Application
import           Application.User
import           Acid.API
import           Acid.Auth
import           HtmlUI
------------------------------------------------------------------------------
-- import           Utils.Debug
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Render login form
handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = maybe mempty splice authError
    splice err = "loginError" ## I.textSplice err


------------------------------------------------------------------------------
-- | Handle login submit
handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit =
    loginUser "login" "password" Nothing
              (\_ -> handleLogin err) (redirect "/")
  where
    err = Just "Unknown user or password"


------------------------------------------------------------------------------
-- | Logs out and redirects the user to the site index.
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"


------------------------------------------------------------------------------
-- | Handle new user form submit
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = do
    method GET handleForm <|> method POST handleFormSubmit
    -- createCheckpoint
  where
    handleForm = render "new_user"
    handleFormSubmit = registerUser "login" "password" >> redirect "/"

------------------------------------------------------------------------------
-- | Handle edit user form submit
handleEditUser :: String -> Handler App App ()
handleEditUser postAction = do
    auth <- with authacid $ getsSnapletState (^. snapletValue)
    mbauser <- with authacid $ currentUser
    case -- trpp "handleEditUser.activeUser"
         mbauser of
      Just auser -> do
        mbuser <- with acid $ query $ UserLookupByNameAcid (userLogin auser)
        case mbuser of
          Just user -> do
            (formView, formResult) <- DS.runForm "form" $ editFormUser user
            case formResult of
              Just user2 -> do
                with acid $ update $ UserSetByKeyAcid (fromJust $ userId $ user ^. authUser) user2
                dflt
              _ -> heistLocal (bindDigestiveSplices formView)
                     $ renderWithSplices "userEditForm"
                     $ "postAction" ## (I.textSplice $ T.pack postAction)
          _ -> dflt
      _ -> dflt
  where
    dflt = redirect "/"

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/login"        , with authacid handleLoginSubmit)
         , ("/logout"       , with authacid handleLogout)
         , ("/home"         , render "mainMenu")
         , mkFormRoute "edit_user" handleEditUser
         , ("/new_user"     , with authacid handleNewUser)
         , (""              , serveDirectory "static")
         ]
  where mkFormRoute :: String -> (String -> Handler App App ()) -> (ByteString, Handler App App ())
        mkFormRoute formName h = (B.pack postAction, (h postAction))
          where postAction = "/" ++ formName


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)

{-
    -- NOTE: We're using initJsonFileAuthManager here because it's easy and
    -- doesn't require any kind of database server to run.  In practice,
    -- you'll probably want to change this to a more robust auth backend.
    a  <- nestSnaplet "auth" auth $
            initJsonFileAuthManager defAuthSettings sess "users.json"
-}
    c  <- nestSnaplet "acid" acid $ acidInit (emptyAppAcid)
    ac <- nestSnaplet "auth-acid" authacid $
            initAcidAuthManager defAuthSettings (_acidStore $ c ^. snapletValue) sess
    addRoutes routes
    addAuthSplices h authacid
    return $ App h s ac c

