------------------------------------------------------------------------------
-- | Various utils for PP
module Utils.Pretty
  -- exports all
  where

------------------------------------------------------------------------------
import           Data.Text
import           Data.ByteString.Char8 as B
import           UHC.Util.Pretty
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- PP instances
instance PP Text where
  pp = pp . show

instance PP ByteString where
  pp = pp . B.unpack

instance PP () where
  pp _ = text "()"

