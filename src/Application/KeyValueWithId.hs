-- {-# LANGUAGE TypeFamilies #-}

------------------------------------------------------------------------------
-- | This module defines state for key+value with automatic id gen for key
module Application.KeyValueWithId
  -- all is exported
  where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad.State
import           Control.Monad.Reader
import qualified Data.Map as Map
------------------------------------------------------------------------------
import           UHC.Util.Pretty
import           Utils.Debug
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Per user maintained persistent state
class (Ord (Id it), Ord (Key it)) => KeyValueWithId it where
  -- | Identification
  type Id it        :: *
  -- | Key
  type Key it       :: *
  -- | Value
  type Val it       :: *
  -- | Lens into mapping from Key to Id
  kviKey2IdLens     :: Lens' it (Map.Map (Key it) (Id it))
  -- | Lens into mapping from Id to Value
  kviId2ValLens     :: Lens' it (Map.Map (Id it) (Val it))
  -- | Generate next Id
  kviNextId         :: (Monad m, MonadState it m) => m (Id it)
  -- | Extract Key+Id
  kviKeyIdOfVal :: (Monad m, MonadState it m) => Val it -> m (Maybe (Key it), Id it)
  -- | Additional post processing of Key+Id+Val before deletion
  kviPreDeleteVal :: (Monad m, MonadState it m) => Val it -> m ()
  kviPreDeleteVal _ = return ()
  -- | Additional post processing of Key+Id+Val after addition
  kviPostInsertVal :: (Monad m, MonadState it m) => Id it -> Val it -> m ()
  kviPostInsertVal _ _ = return ()

------------------------------------------------------------------------------
-- Derived API

-- | Add a key + value, state monadically
kviAdd :: (Monad m, MonadState it m, KeyValueWithId it) => (Key it -> Id it -> val' -> Val it) -> Key it -> val' -> m (Maybe (Id it))
kviAdd embed k val' = do
    m <- use kviKey2IdLens
    case Map.lookup k m of
      Just _ -> return Nothing
      _ -> do
        newId <- kviNextId
        kviKey2IdLens %= Map.insert k newId
        let val = embed k newId val'
        kviId2ValLens %= Map.insert newId val
        kviPostInsertVal newId val
        return $ Just newId

-- | Delete the value
kviDelete :: (Monad m, MonadState it m, KeyValueWithId it) => Val it -> m ()
kviDelete val = do
    (k,i) <- kviKeyIdOfVal val
    kviPreDeleteVal val
    kviKey2IdLens  %= maybe id Map.delete k
    kviId2ValLens  %=          Map.delete i

-- | Update, return the updated
kviUpdateById
  :: ( Monad m, MonadState it m, KeyValueWithId it
     , PP (Id it), PP (Val it), PP (Key it)
     ) => Id it -> (Val it -> Val it) -> m (Maybe (Val it))
kviUpdateById i upd = do
    m <- use kviId2ValLens
    case (\v -> trpp' "kviUpdateById.lkp" (i >#< v) v) $
         Map.lookup i m of
      Just val -> do
        (mk , i ) <- kviKeyIdOfVal val
        let val' = upd val
        (mk', i') <- kviKeyIdOfVal val'
        kviPreDeleteVal val
        kviId2ValLens %= Map.delete i
        kviId2ValLens %= Map.insert i' val'
        case (mk, mk') of
          (Just k , Just k') -> kviKey2IdLens %= (Map.insert k' i' . Map.delete k)
          (Just k , Nothing) -> kviKey2IdLens %= (                   Map.delete k)
          (Nothing, Just k') -> kviKey2IdLens %= (Map.insert k' i'               )
          _                  -> return ()
        kviPostInsertVal i' val'
        return $ Just val'
      Nothing -> return Nothing

-- | Lookup user by id
kviLookupById' :: (KeyValueWithId it) => Id it -> it -> Maybe (Val it)
kviLookupById' i it = Map.lookup i (it ^. kviId2ValLens)

-- | Lookup user by id
kviLookupById :: (Functor m, Monad m, MonadReader it m, KeyValueWithId it) => Id it -> m (Maybe (Val it))
kviLookupById i = fmap (kviLookupById' i) $ ask

-- | Lookup user by key
kviLookupByKey :: (Functor m, Monad m, MonadReader it m, KeyValueWithId it) => Key it -> m (Maybe (Val it))
kviLookupByKey k = do
    mi <- fmap (Map.lookup k) $ view kviKey2IdLens
    maybe (return Nothing) kviLookupById mi
