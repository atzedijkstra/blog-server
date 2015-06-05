-- {-# LANGUAGE AllowAmbiguousTypes #-}

------------------------------------------------------------------------------
-- | Various utils
module Utils
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
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Restricted form of zoom to stick with same monad
-- zoom :: (MonadState s m, MonadState t m) => L.LensLike' (L.Zoomed m c) t s -> m c -> m c
-- zoom :: (Monad m) => L.LensLike' (L.Zoomed (StateT t m) c) t s -> StateT t m c -> StateT s m c
-- zoom :: (MonadState s m, MonadState t m) => L.Lens' s t -> m c -> m c
-- zoom :: (Monad m) => L.Lens' s t -> StateT t m c -> StateT s m c
{-
zoom :: (MonadState s m, MonadState t n) => L.Lens' s t -> n c -> m c
zoom = L.zoom
-}

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
