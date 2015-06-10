{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module defines additional SafeCopy stuff
module Utils.SafeCopy
  -- all is exported
  where

------------------------------------------------------------------------------
import           Data.Typeable
import           Snap.Snaplet.Auth
import           Data.SafeCopy
import qualified UHC.Util.RelMap as Rel
import qualified Data.HashMap.Strict as H
import           Data.Hashable (Hashable)
import           Data.Aeson (Value)
import           Data.Scientific(Scientific)
------------------------------------------------------------------------------
import           Config.SafeCopy
------------------------------------------------------------------------------
-- import           Utils.Debug
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- AuthUser

deriving instance Typeable UserId
deriving instance Typeable AuthUser

instance (SafeCopy a, SafeCopy b, Eq a, Hashable a) =>
    SafeCopy (H.HashMap a b) where
      getCopy = contain $ fmap H.fromList safeGet
      putCopy = contain . safePut . H.toList

deriveSafeCopy safeCopyVersion 'base ''UserId
deriveSafeCopy safeCopyVersion 'base ''AuthUser
deriveSafeCopy safeCopyVersion 'base ''Password
deriveSafeCopy safeCopyVersion 'base ''Role
deriveSafeCopy safeCopyVersion 'base ''AuthFailure

deriveSafeCopy safeCopyVersion 'base ''Value

deriveSafeCopy safeCopyVersion 'base ''Scientific

------------------------------------------------------------------------------
-- RelMap

instance (SafeCopy a, SafeCopy b, Ord a, Ord b) =>
    SafeCopy (Rel.Rel a b) where
      getCopy = contain $ fmap Rel.fromList safeGet
      putCopy = contain . safePut . Rel.toList
