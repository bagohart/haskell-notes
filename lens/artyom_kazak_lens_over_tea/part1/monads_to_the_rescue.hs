{-# LANGUAGE RankNTypes #-}

import Control.Monad
import Data.Bifunctor

-- the example doesn't work:
-- type Lens s a = Monad m => (a -> m a) -> s -> (a, m s)
-- probably this has been deprecated (?_?)

-- type Lens s a = (a -> a) -> s -> (a, s)
-- ^ last attempt:
type Lens m s a = (a -> m a) -> s -> (a, m s)
-- m = [] is a Monad, so...
-- type Lens [] s a = (a -> [a]) -> [a] -> (a, [[a]])

ix :: Int -> (a -> [a]) -> [a] -> (a, [[a]])
ix index f list
    | index < 0 = error "ix: negative index :("
    | null list = error "ix: index too large :("
    | old:rest <- list = if index == 0
                            then (old, liftM (: rest) (f old)) 
                            else second (liftM (old :)) $ ix (index-1) f rest

