-- {-# LANGUAGE AllowAmbiguousTypes #-}

------------------------------------------------------------------------------
-- | Various utils for Monad
module Utils.Monad
  ( -- zoom
    liftST2MS
  , liftRT2MR
  )
  where

------------------------------------------------------------------------------
import           Control.Monad.State as MS
import           Control.Monad.Reader as MR
-- import qualified Control.Lens as L
-- import qualified Control.Lens.Internal.Zoom as L
import           Data.Text
import           UHC.Util.Pretty
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- PP instances
instance PP Text where
  pp = pp . show

------------------------------------------------------------------------------
-- | Lift from StateT to a MonadState
liftST2MS :: MonadState s m => StateT s m y -> m y
liftST2MS m = do
  ma <- get
  (res,ma2) <- runStateT m ma
  put ma2
  return res

------------------------------------------------------------------------------
-- | Lift from ReaderT to a MonadReader
liftRT2MR :: MonadReader s m => ReaderT s m y -> m y
liftRT2MR m = do
  ma <- ask
  (res) <- runReaderT m ma
  return res
