------------------------------------------------------------------------------
-- | This module provides config info
module Config.SafeCopy
  where

------------------------------------------------------------------------------
import           Data.SafeCopy
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | SafeCopy config (must be here, enforced by TH to be outside module where used)
safeCopyVersion :: Version a
safeCopyVersion = 0
