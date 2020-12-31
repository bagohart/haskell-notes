{-# LANGUAGE InstanceSigs #-}

import Control.Applicative

data User = User deriving Show

findById :: Int -> IO (Maybe User)
findById 1 = return $ Just User
findById _ = return Nothing

findUsers :: Int -> Int -> IO (Maybe (User,User))
findUsers x y = do
    muser1 <- findById x
    case muser1 of
      Nothing -> return Nothing
      Just user1 -> do
          muser2 <- findById y
          case muser2 of
            Nothing -> return Nothing
            Just user2 -> do
                return $ Just (user1, user2)

-- ^ this would all be much simpler if findById didn't have side effects:
findByIdPure :: Int -> Maybe User
findByIdPure 1 = Just User
findByIdPure _ = Nothing

loadUsers :: Maybe (User,User)
loadUsers = do
    user1 <- findByIdPure 1
    user2 <- findByIdPure 2
    return (user1,user2)

-- actually, I think I could applicative this:
loadUsers2 :: Maybe (User,User)
loadUsers2 = (,) <$> findByIdPure 1 <*> findByIdPure 2

-- now let's teach failure to IO.
data MaybeIO a = MaybeIO { runMaybeIO :: IO (Maybe a) }

instance Functor MaybeIO where 
    fmap :: (a -> b) -> MaybeIO a -> MaybeIO b
    fmap g (MaybeIO m) = MaybeIO $ (fmap . fmap) g m

instance Applicative MaybeIO where 
    pure :: a -> MaybeIO a
    pure x = MaybeIO $ return (Just x)

    (<*>) :: MaybeIO (a -> b) -> MaybeIO a -> MaybeIO b
    (<*>) (MaybeIO mg) (MaybeIO ma) = MaybeIO $ liftA2 (<*>) mg ma
    -- which should be like:
    -- (<*>) (MaybeIO mg) (MaybeIO ma) = MaybeIO $ (<*>) <$> mg <*> ma

instance Monad MaybeIO where 
    return = pure
    (>>=) :: MaybeIO a -> (a -> MaybeIO b) -> MaybeIO b
    (>>=) (MaybeIO ioma) g = MaybeIO $ do
        ma <- ioma
        case ma of
          Nothing -> return Nothing
          Just a -> runMaybeIO $ g a

smartFindUsers :: Int -> Int -> MaybeIO (User,User)
smartFindUsers x y = do
    user1 <- MaybeIO $ findById x
    user2 <- MaybeIO $ findById y
    return (user1,user2)

-- or, keeping the original type:
smartFindUsers2 :: Int -> Int -> IO (Maybe (User,User))
smartFindUsers2 x y = runMaybeIO $ do
    user1 <- MaybeIO $ findById x
    user2 <- MaybeIO $ findById y
    return (user1,user2)

-- again, I think I can Applicative this: (but I need another function first)
findById2 :: Int -> MaybeIO User
findById2 1 = return User
findById2 _ = MaybeIO $ return Nothing

smartFindUsers3 :: Int -> Int -> IO (Maybe (User,User))
smartFindUsers3 x y = runMaybeIO $ (,) <$> findById2 x <*> findById2 y

-- or with the Monad Transformer type:
smartFindUsersT :: Int -> Int -> MaybeIO (User,User)
smartFindUsersT x y = (,) <$> findById2 x <*> findById2 y

-- and now the general MaybeT thingy:
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
-- and more blabla. know already.
