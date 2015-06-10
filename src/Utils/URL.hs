------------------------------------------------------------------------------
-- | Various utils for URL construction
module Utils.URL
  -- export all
  where

------------------------------------------------------------------------------
import           Data.List
import           Data.String
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Key/Value pair

-- | construct key/val pair param
urlMkKeyValParam :: String -> String -> String
urlMkKeyValParam k v = k ++ "=" ++ v

-- | append all params
urlSuffixParams :: [String] -> String -> String
urlSuffixParams kvs url = url ++ "?" ++ intercalate "&" kvs

