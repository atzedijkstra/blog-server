{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module defines per blog maintained state
module Application.Blog
  -- all is exported
  where

------------------------------------------------------------------------------
import           Data.Typeable
import           Control.Lens
import           Control.Monad.State
import           Control.Monad.Reader
import           Data.SafeCopy
import qualified Data.Map as Map
import qualified Data.Text as T
import           UHC.Util.Pretty
------------------------------------------------------------------------------
import           Config.SafeCopy
import           Utils.Pretty()
import           Application.KeyValueWithId
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Per blog maintained persistent state
-- | Identification for blog
type BlogKey    = T.Text
type BlogKeyGen = Int
type BlogName   = T.Text

data Blog = Blog
    { _blogId           :: BlogKey
    , _blogTitle        :: BlogName
    , _blogContent      :: T.Text
    }
    deriving (Typeable)

emptyBlog :: Blog
emptyBlog = Blog "" "" ""

makeLenses ''Blog

deriveSafeCopy safeCopyVersion 'base ''Blog

instance Show Blog where
  show _ = "Blog"

instance PP Blog where
  pp (Blog {_blogTitle=t, _blogContent=c}) = t >-< c

------------------------------------------------------------------------------
-- Mapping from blog to state

-- | Relation between Name, Key and Blog
data Blogs = Blogs
    { _blogsNextKey        :: !BlogKeyGen
    , _blogsName2KeyMp     :: Map.Map BlogName BlogKey
    , _blogsKey2BlogMp     :: Map.Map BlogKey Blog
    }
    deriving (Typeable)

makeLenses ''Blogs

emptyBlogs :: Blogs
emptyBlogs = Blogs 0 Map.empty Map.empty

deriveSafeCopy safeCopyVersion 'base ''Blogs

instance Show Blogs where
  show _ = "Blogs"

instance PP Blogs where
  pp (Blogs {_blogsNextKey=nk, _blogsName2KeyMp=n2k, _blogsKey2BlogMp=k2u}) =
    text "Blogs" >#< ppParens nk >-< indent 2
      ( Map.toList k2u >-< Map.toList n2k
      )

instance KeyValueWithId Blogs where
  type Id Blogs     = BlogKey
  type Key Blogs    = BlogName
  type Val Blogs    = Blog
  kviKey2IdLens     = blogsName2KeyMp
  kviId2ValLens     = blogsKey2BlogMp
  kviNextId         = blogsNextKey %%= \k -> (T.pack $ show k, k+1)
  kviKeyIdOfVal b   = return (Just $ b ^. blogTitle, b ^. blogId) 

------------------------------------------------------------------------------
-- Functionality on Blog and Blogs

-- | Add a blog, state monadically
blogAdd :: (Monad m, MonadState Blogs m) => BlogName -> m (Maybe BlogKey)
blogAdd nm = kviAdd embed nm ()
  where
    embed nm newKey _ = ((blogTitle .~ nm) . (blogId .~ newKey)) emptyBlog

-- | Delete a blog
blogDelete :: (Monad m, MonadState Blogs m) => Blog -> m ()
blogDelete = kviDelete

-- | Update blog, return updated Blog
blogUpdateByKey :: (Monad m, MonadState Blogs m) => BlogKey -> (Blog -> Blog) -> m (Maybe Blog)
blogUpdateByKey = kviUpdateById

-- | Lookup blog by key
blogLookupByKey :: (Functor m, Monad m, MonadReader Blogs m) => BlogKey -> m (Maybe Blog)
blogLookupByKey = kviLookupById

-- | Lookup blog by name
blogLookupByName :: (Functor m, Monad m, MonadReader Blogs m) => BlogName -> m (Maybe Blog)
blogLookupByName = kviLookupByKey

-- | Blogs as a list, sorted on order of entry
blogsToList :: Blogs -> [Blog]
blogsToList = Map.elems . _blogsKey2BlogMp
