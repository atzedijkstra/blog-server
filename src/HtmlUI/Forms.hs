{-# LANGUAGE OverloadedStrings, TemplateHaskell, ParallelListComp #-}

------------------------------------------------------------------------------
-- | This module is where the Html (form) interfaces are
module HtmlUI.Forms
  -- all is exported
  where

------------------------------------------------------------------------------
import           Data.Maybe
-- import qualified Data.Map as Map
-- import qualified Data.Set as Set
import qualified Data.ByteString.Char8 as B
import           Control.Lens
-- import           Data.Monoid
import qualified Data.Text as T
import           Control.Monad
-- import           Control.Monad.State
import           Control.Applicative
-- import qualified Prelude as P
import           Prelude
import           Snap.Snaplet.Auth (AuthUser(..), Password(..), encryptPassword)
import           System.IO.Unsafe (unsafePerformIO)
------------------------------------------------------------------------------
-- import           Text.Blaze.Html
-- import           Text.Blaze.Html5
-- import           Text.Blaze.Html5.Attributes
-- import qualified Text.Blaze.Html5 as H
-- import qualified Text.Blaze.Html5.Attributes as A
-- import           Text.Blaze.Renderer.XmlHtml
import           Text.Digestive as D
-- import           Text.Digestive.Blaze.Html5
-- import           Text.Digestive.Heist
-- import           Text.Templating.Heist
-- import qualified Text.XmlHtml as X
------------------------------------------------------------------------------
import           Application.User
import           Application.Blog
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Utils

-- | Check non emptiness of form field
checkNonEmpty :: (Monad m) => {- v -> (a -> Bool) -> -} Form T.Text m T.Text -> Form T.Text m T.Text
checkNonEmpty = check "Must be non-empty" (not . T.null)

------------------------------------------------------------------------------
-- | User form
editFormUser :: Monad m => User -> Form T.Text m User
editFormUser u =
    (\n pw e ->
      (userName .~ n) .
      (authUser %~ \au -> au
           { userEmail = Just e
           , userPassword = if T.null pw then userPassword au else (Just $ encr $ ClearText $ B.pack $ T.unpack pw)
           }) $
        u
    )
    <$> "name"      .: checkNonEmpty (D.text (Just $ u ^. userName))
    <*> "password"  .: D.text Nothing
    <*> "email"     .: check "Not a valid email address" checkEmail (D.text (userEmail $ u ^. authUser))
  where
    checkEmail e | T.null e  = True
                 | otherwise = isJust $ T.find (== '@') e
    encr pw = unsafePerformIO $ encryptPassword pw

------------------------------------------------------------------------------
-- | Blog form
editFormBlog :: Monad m => Blog -> Form T.Text m Blog
editFormBlog b =
    (\t c ->
      (blogTitle .~ t) .
      (blogContent .~ c) $
        b
    )
    <$> "title"      .: checkNonEmpty (D.text (Just $ b ^. blogTitle))
    <*> "content"    .: D.text (Just $ b ^. blogContent)

