{-# LANGUAGE InstanceSigs #-}

import Data.Functor.Compose
import Data.Functor.Const
import Data.Functor.Identity

type Lens f s t a b = (a -> f b) -> s -> f t

-- ix has become a bit simpler:
ix :: Functor f => Int -> Lens f [a] [a] a a
ix index f list
    | index < 0 = error "ix: negative index :("
    | null list = error "ix: index too large :("
    | old:rest <- list = if index == 0
                            then (: rest) <$> (f old) 
                            else (old :) <$> ix (index-1) f rest


-- this is a functor. also it stores a value of type x. mm hm.
data Storey x f a = Storey x (f a) deriving Show
-- so basically, this is isomorphic to (x, f a)

instance Functor f => Functor (Storey x f) where 
    fmap :: (a -> b) -> Storey x f a -> Storey x f b
    fmap g (Storey x fa) = Storey x (g <$> fa)-- how to get a value:

getByStorey lens s = x
    where 
        Storey x _ = lens (\x -> Storey x (Identity x)) s

getByConst lens s = x
    where 
        Const x = lens (\x -> Const x) s

-- use this:
getIt1 = getByStorey (ix 2) [0..4]
getIt2 = getByConst (ix 2) [0..4]

-- the tutorial contains a great metaphor about Const :)
