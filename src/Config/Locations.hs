------------------------------------------------------------------------------
-- | This module provides config info for globally assumed/known url & names
module Config.Locations
  -- export all
  where

------------------------------------------------------------------------------
-- import           Data.SafeCopy
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Handler name entry points

-- | Edit blog
handlernameEditBlog :: String
handlernameEditBlog = "editBlog"

------------------------------------------------------------------------------
-- Handler param name

-- | Edit blog
handlernameBlogId :: String
handlernameBlogId = "blogId"

------------------------------------------------------------------------------
-- Utils

-- | Make top level handler name
handlernameMkTop :: String -> String
handlernameMkTop s@('/':_) = s
handlernameMkTop s         = "/" ++ s

