------------------------------------------------------------------------------
-- | Various utils for debugging
module Utils.Debug
  ( trpp
  , trpp'
  )
  where

------------------------------------------------------------------------------
import           Debug.Trace
import           UHC.Util.Pretty
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | Trace & PP
trpp' :: PP x => String -> x -> a -> a
trpp' msg x res = trace (show $ msg >|< ":" >#< x) res

trpp :: PP x => String -> x -> x
trpp msg x = trpp' msg x x
