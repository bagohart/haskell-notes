{-# LANGUAGE BangPatterns, DeriveFunctor #-}

import Control.Monad.State

sumTillNegative :: [Int] -> Int
sumTillNegative = go 0
    where 
        go !total rest = case rest of
                           [] -> total
                           x:xs
                             | x < 0 -> total
                             | otherwise -> go (total + x) xs

main :: IO ()
main = print $ sumTillNegative [1,2,3,-1,4]

foldTerminate :: (b -> a -> Either b b) -> b -> [a] -> b
foldTerminate f = go
    where 
        go !accum rest = case rest of
                           [] -> accum
                           x:xs -> case f accum x of
                                     Left accum' -> accum'
                                     Right accum' -> go accum' xs

sumTillNegative2 = foldTerminate go 0
    where go total x
            | x < 0 = Left total
            | otherwise = Right (total + x)

main2 = print $ sumTillNegative2 [1,2,3,-1,4]

foldTerminate2 :: (b -> a -> Either b b) -> b -> [a] -> b
foldTerminate2 f accum0 list0 = either id id (go accum0 list0)
        where 
            go !accum rest = do
                (x,xs) <- case rest of
                            [] -> Left accum -- this terminates the loop.
                            x:xs -> Right (x,xs)
                accum' <- f accum x
                go accum' xs

-- comment out to prevent dependency that would clash with following reimplementation...
-- foldState :: (b -> a -> b) -> b -> [a] -> b
-- foldState f accum0 list0 = execState (mapM_ go list0) accum0
--     where go x = modify' (\accum -> f accum x)

-- so we can:
-- simplify terminating the loop with Either
-- simplify folding the thing with State
-- how to combine?

newtype StateEither s e a = StateEither { runStateEither :: s -> (s, Either e a) } deriving Functor

instance Applicative (StateEither s e) where 
    pure a = StateEither (\s -> (s, Right a))
    StateEither ff <*> StateEither fa = StateEither $ \s0 ->
        case ff s0 of
          (s1, Left e) -> (s1, Left e)
          (s1, Right f) ->
                case fa s1 of
                  (s2, Left e) -> (s2, Left e)
                  (s2, Right a) -> (s2, Right (f a))

instance Monad (StateEither s e) where
    return = pure 
    StateEither f >>= g = StateEither $ \s0 ->
        case f s0 of
          (s1, Left e) -> (s1, Left e)
          (s1, Right x) -> runStateEither (g x) s1

execStateEither :: StateEither s e a -> s -> s
execStateEither m = fst . runStateEither m

modify'' :: (s -> Either e s) -> StateEither s e ()
modify'' f = StateEither $ \s0 ->
    case f s0 of
      Left e -> (s0, Left e) -- doesn't modify the state, gives a result
      Right !s1 -> (s1, Right ()) -- modifies the state, gives no result o_O

foldTerminate3 :: (b -> a -> Either b b) -> b -> [a] -> b
foldTerminate3 f accum0 list0 = execStateEither (mapM_ go list0) accum0
    where go x = modify'' (\accum -> f accum x)

-- reformulate StateEither
newtype StateEither2 s e a = StateEither2 { unStateEither :: State s (Either e a) } deriving Functor

-- and we can also use the State monad instance in our implementation, not directly touching the state:
instance Applicative (StateEither2 s e) where 
    pure a = StateEither2 $ return $ Right a
    StateEither2 ff <*> StateEither2 fa = StateEither2 $ do
        ef <- ff
        case ef of
          Left e -> return $ Left e
          Right f -> do
              ea <- fa
              case ea of
                Left e -> return $ Left e
                Right a -> return $ Right $ f a
-- ^ here, we basically only care for the Either part

instance Monad (StateEither2 s e) where 
    return = pure 
    StateEither2 f >>= g = StateEither2 $ do
        ex <- f
        case ex of
          Left e -> return $ Left e
          Right x -> unStateEither $ g x

execStateEither2 :: StateEither2 s e a -> s -> s
execStateEither2 (StateEither2 m) s = execState m s

modify2' :: (s -> Either e s) -> StateEither2 s e ()
modify2' f = StateEither2 $ do
    s0 <- get
    case f s0 of
      Left e -> return $ Left e
      Right s1 -> do
          put $! s1
          return $ Right ()

-- now general version

newtype EitherT e m a = EitherT (m (Either e a)) deriving Functor

runEitherT (EitherT x) = x

execStateEither3 :: EitherT e (State s) a -> s -> s
execStateEither3 (EitherT m) s = execState m s

modify4 :: (s -> Either e s) -> EitherT e (State s) ()
modify4 f = EitherT $ do
  s0 <- get
  case f s0 of
    Left e -> return $ Left e
    Right s1 -> do
      put $! s1
      return $ Right ()

instance Monad m => Applicative (EitherT e m) where
    pure a = EitherT $ return $ Right a
    EitherT ff <*> EitherT fa = EitherT $ do
      ef <- ff
      case ef of
        Left e -> return $ Left e
        Right f -> do
          ea <- fa
          case ea of
            Left e -> return $ Left e
            Right a -> return $ Right $ f a

instance Monad m => Monad (EitherT e m) where
    return = pure
    EitherT f >>= g = EitherT $ do
      ex <- f
      case ex of
        Left e -> return $ Left e
        Right x -> runEitherT $ g x

exitEarly :: Monad m => e -> EitherT e m a
exitEarly e = EitherT $ return $ Left e

myLift :: Monad m => m a -> EitherT e m a
myLift action = EitherT $ fmap Right $ action

modify5 :: (s -> Either e s) -> EitherT e (State s) ()
modify5 f = do
    s0 <- myLift get
    case f s0 of
      Left e -> exitEarly e
      Right s1 -> myLift $ put $! s1
