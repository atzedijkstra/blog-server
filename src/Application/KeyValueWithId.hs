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

{-
-}

-- | Update, return the updated
kviUpdateById :: (Monad m, MonadState it m, KeyValueWithId it) => Id it -> (Val it -> Val it) -> m (Maybe (Val it))
kviUpdateById i upd = do
    m <- use kviId2ValLens
    case Map.lookup i m of
      Just val -> do
        (mk ,i ) <- kviKeyIdOfVal val
        let val' = upd val
        (mk',i') <- kviKeyIdOfVal val'
        kviPreDeleteVal val
        unless (i == i') $ do
          kviId2ValLens %= Map.delete i
          kviId2ValLens %= Map.insert i' val'
        case (mk, mk') of
          (Just k , Just k') | k /= k'   -> kviKey2IdLens %= (Map.insert k' i . Map.delete k)
          (Just k , Nothing)             -> kviKey2IdLens %= (                  Map.delete k)
          (Nothing, Just k')             -> kviKey2IdLens %= (Map.insert k' i               )
          _                              -> return ()
        kviPostInsertVal i' val'
        return $ Just val'
      Nothing -> return Nothing
{-
-- | Update user, return updated User
userUpdateByKey :: (Monad m, MonadState Users m) => UserKey -> (User -> User) -> m (Maybe User)
userUpdateByKey k upd = do
{-
    m <- use usersKey2UserMp
    case Map.lookup k m of
      Just u -> do
        let u' = upd u
        case (u ^. userName, u' ^. userName) of
          (n, n') | n == n'   -> return ()
                  | otherwise -> usersName2KeyMp  %= (Map.insert n' k . Map.delete n)
        case (userRememberToken $ u ^. authUser, userRememberToken $ u' ^. authUser) of
          (Just t , Just t') | t /= t'   -> usersToken2KeyMp %= (Map.insert t' k . Map.delete t)
          (Just t , Nothing)             -> usersToken2KeyMp %= (                  Map.delete t)
          (Nothing, Just t')             -> usersToken2KeyMp %= (Map.insert t' k               )
          _                              -> return ()
        return $ Just u'
      Nothing -> return Nothing
-}
{-
-}
    -- incorrect because various keys can change, but seems to work and above not... Sigh...
    usersKey2UserMp %= Map.update (Just. upd) k
    m <- use usersKey2UserMp
    return $ Map.lookup k m 
{-
    m <- use usersKey2UserMp
    case Map.lookup k m of
      Just u -> do
        let u' = upd u
            dnm = Map.delete (u ^. userName)
            inm = Map.insert (u' ^. userName) k
            dtk = maybe id (Map.delete) (userRememberToken $ u ^. authUser)
            itk = maybe id (\tk -> Map.insert tk k) (userRememberToken $ u' ^. authUser)
        usersName2KeyMp  %= (inm . dnm)
        usersToken2KeyMp %= (itk . dtk)
        return $ Just u'
      Nothing -> return Nothing
-}
-}

{-
-- | Lookup user by key
userLookupByKey :: (Functor m, Monad m, MonadReader Users m) => UserKey -> m (Maybe User)
userLookupByKey k = fmap (Map.lookup k) $ view usersKey2UserMp

-- | Lookup user by name
userLookupByName :: (Functor m, Monad m, MonadReader Users m) => UserName -> m (Maybe User)
userLookupByName nm = do
    mk <- fmap (Map.lookup nm) $ view usersName2KeyMp
    maybe (return Nothing) userLookupByKey mk

-- | Lookup user by rtoken
userLookupByRToken :: (Functor m, Monad m, MonadReader Users m) => UserRToken -> m (Maybe User)
userLookupByRToken rt = do
    mk <- fmap (Map.lookup rt) $ view usersToken2KeyMp
    maybe (return Nothing) userLookupByKey mk
-}
