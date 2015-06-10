{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module defines the forms used by the app
module HtmlUI.Render
  -- all is exported
  where

------------------------------------------------------------------------------
-- import           Data.Maybe
-- import           Data.Monoid
import           Control.Monad
import           Control.Lens
-- import qualified Data.Text as T
import qualified Text.Blaze.Html5 as H
------------------------------------------------------------------------------
import           HtmlUI.DTable
-- import           Application
import           Application.Blog
import           Application.User
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Blog rendering

-- | Render Blogs to Html
hrenderBlogs :: (Blog -> H.Html) -> [(Blog,User)] -> H.Html
hrenderBlogs mkForId blogAndUsers = do
    forM_ (reverse blogAndUsers) $ \(blog,user) -> do
      H.br
      dtableToHtml Nothing Nothing
        [ [ H.toMarkup $ user ^. userName
          , H.toMarkup $ blog ^. blogTitle
          ]
        , [ mkForId blog
          , H.toMarkup $ blog ^. blogContent
          ]
        ]
