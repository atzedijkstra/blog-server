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
import           Data.String
import           Control.Lens
import           Control.Applicative
import           Data.ByteString.Char8 as B (pack, unpack, ByteString)
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
import           UHC.Util.Pretty
------------------------------------------------------------------------------
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import           Text.Blaze.Renderer.XmlHtml
------------------------------------------------------------------------------
import qualified Text.Digestive.Snap as DS
import           Text.Digestive.Heist
------------------------------------------------------------------------------
import           Application
import           Application.User
import           Application.Blog
import           Acid.API
import           Acid.Auth
import           HtmlUI.Forms
import           HtmlUI.Render
import           Config.Locations
import           Utils.Monad
import           Utils.URL
------------------------------------------------------------------------------
import           Utils.Debug
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Utils

-- | Current user
getCurrentUser :: Handler App App (Maybe User)
getCurrentUser = do
    mbauser <- with authacid $ currentUser
    case mbauser of
      Just auser -> with acid $ query $ UserLookupByNameAcid (userLogin auser)
      _          -> return Nothing

-- | Default handler for various pieces of code
defaultHandler :: Handler App v ()
defaultHandler = redirect "/home"

-- | 
mkEditBlogHdlrNm :: String -> T.Text -> String
mkEditBlogHdlrNm hdlrNm blogId = urlSuffixParams [urlMkKeyValParam handlernameBlogId (T.unpack blogId)] hdlrNm

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
              (\_ -> handleLogin err) defaultHandler
  where
    err = Just "Unknown user or password"


------------------------------------------------------------------------------
-- | Logs out and redirects the user to the site index.
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> defaultHandler


------------------------------------------------------------------------------
-- | Handle new user form submit
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = do
    method GET handleForm <|> method POST handleFormSubmit
    -- createCheckpoint
  where
    handleForm = render "newUser"
    handleFormSubmit = registerUser "login" "password" >> defaultHandler

------------------------------------------------------------------------------
-- | Handle edit user form submit
handleEditUser :: String -> Handler App App ()
handleEditUser postAction = do
    mbuser <- getCurrentUser
    case mbuser of
      Just user -> do
        let uid = userKey user
        (formView, formResult) <- DS.runForm "form" $ editFormUser user
        case formResult of
          Just user2 -> do
            with acid $ update $ UserSetByKeyAcid uid user2
            defaultHandler
          _ -> do
            blogAndUsers <- query $ BlogsSelectByMbUserAcid (Just uid)
            heistLocal (bindDigestiveSplices formView)
              $ renderWithSplices "userEditForm" $ do
                  "postAction" ## (I.textSplice $ T.pack postAction)
                  "blogs" ##
                    (return $ renderHtmlNodes $ flip hrenderBlogs blogAndUsers $ \blog ->
                       H.a H.! HA.href (fromString $ handlernameMkTop $ mkEditBlogHdlrNm handlernameEditBlog (blog ^. blogId))
                         $ "edit"
                    )
      _ -> defaultHandler

------------------------------------------------------------------------------
-- | Handle edit blog form submit
handleEditBlog :: String -> Handler App App ()
handleEditBlog postAction = do
    mbuser <- getCurrentUser
    mbBlogId@(~(Just blogId')) <- getParam $ B.pack handlernameBlogId
    let isOnExistingBlog = isJust mbBlogId
        blogId = T.pack $ trpp "handleEditBlog.blogId" $ B.unpack blogId'
    case mbuser of
      Just user -> do
        blog <- if isOnExistingBlog
          then fmap fromJust $ query $ BlogLookupByKeyAcid blogId
          else return emptyBlog
        (formView, formResult) <- DS.runForm "form" $ editFormBlog blog
        case (\v -> trpp' "handleEditBlog.blog2" (mbBlogId >#< v) v) $
             formResult of
          Just blog2 -> do
            with acid $ if (trpp "handleEditBlog.on exists?" isOnExistingBlog)
              then voidM $ update $ BlogSetByKeyAcid blogId blog2
              else voidM $ update $ BlogAddForUserAcid (fromJust $ userId $ user ^. authUser) blog2
            defaultHandler
          _ -> heistLocal (bindDigestiveSplices formView)
                 $ renderWithSplices "blogEditForm"
                 $ "postAction" ## (I.textSplice $ T.pack $ if isOnExistingBlog then mkEditBlogHdlrNm postAction blogId else postAction)
      _ -> defaultHandler

------------------------------------------------------------------------------
-- | Show adm
handleDumpAdm :: Handler App App ()
handleDumpAdm = do
    users <- query UsersAcid
    blogs <- query BlogsAcid
    ownership <- query BlogsOfUserAcid
    renderWithSplices "dump" $ do
      "users" ## mkdump users
      "blogs" ## mkdump blogs
      "ownership" ## mkdump ownership
  where
    mkdump x = return $ renderHtmlNodes $ H.pre $ H.toMarkup $ show (pp x)

------------------------------------------------------------------------------
-- | Show adm
handleHome :: Handler App App ()
handleHome = do
    blogAndUsers <- query $ BlogsSelectByMbUserAcid Nothing
    renderWithSplices "mainMenu" $ do
      "blogs" ## (return $ renderHtmlNodes $ hrenderBlogs (const mempty) blogAndUsers)

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/login"        , with authacid handleLoginSubmit)
         , ("/logout"       , with authacid handleLogout)
         , ("/home"         , handleHome)
         , ("/index"        , handleHome)
         , ("/dump"         , handleDumpAdm)
         , ("/checkpoint"   , createCheckpoint >> defaultHandler)
         , mkFormRoute Nothing "editUserSettings" handleEditUser
         , mkFormRoute Nothing handlernameEditBlog handleEditBlog
         , mkFormRoute (Just handlernameBlogId) handlernameEditBlog handleEditBlog
         , ("/newUser"      , with authacid handleNewUser)
         -- , (""              , handleHome)
         , (""              , serveDirectory "static")
         ]
  where mkFormRoute :: Maybe String -> String -> (String -> Handler App App ()) -> (ByteString, Handler App App ())
        mkFormRoute mbParam formName h = (B.pack $ postAction ++ maybe "" ("/:" ++) mbParam, (h postAction))
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

