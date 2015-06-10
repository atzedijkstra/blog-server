-- {-# LANGUAGE AllowAmbiguousTypes #-}

------------------------------------------------------------------------------
-- | Various utils for Monad
module Utils.Monad
  -- export all
  where

------------------------------------------------------------------------------
import           Control.Monad.State as MS
import           Control.Monad.Reader as MR
------------------------------------------------------------------------------

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

------------------------------------------------------------------------------
-- | Get rid of monadic result
voidM :: Monad m => m x -> m ()
voidM m = m >> return ()
