{-# LANGUAGE InstanceSigs #-}

import Data.Functor.Compose
import Data.Functor.Identity

-- this is a functor. also it stores a value of type x. mm hm.
data Storey x f a = Storey x (f a) deriving Show
-- so basically, this is isomorphic to (x, f a)

instance Functor f => Functor (Storey x f) where 
    fmap :: (a -> b) -> Storey x f a -> Storey x f b
    fmap g (Storey x fa) = Storey x (g <$> fa)

-- this allows us to remove the "original-value-add-on" from our previous lens type:
-- f should be a Functor
type Lens f s a = (a -> f a) -> s -> f s
-- recall, before:
-- type Lens m s a = (a -> m a) -> s -> (a, m s)

-- ix has become a bit simpler:
ix :: Functor f => Int -> Lens f [a] a
ix index f list
    | index < 0 = error "ix: negative index :("
    | null list = error "ix: index too large :("
    | old:rest <- list = if index == 0
                            then (: rest) <$> (f old) 
                            else (old :) <$> ix (index-1) f rest

-- this generates 4 new lists, und also saves the 4 in Storey
theTrick1 = ix 2 (\x -> Storey x [1..x]) [300,100,4,600,900,400]

-- equivalently, we can do this:
theTrick2' = ix 2 (\x -> Compose (x, [1..x])) [300,100,4,600,900,400]
theTrick2 = getCompose $ ix 2 (\x -> Compose (x, [1..x])) [300,100,4,600,900,400]
-- Compose with (a,) and a functor works basically like Storey.
-- The Functor instance is already defined, so I don't have to do it.
-- ix then jumps over the x like in Storey, and works on the second part of the tuple only, because Functor.

-- what if I don't need any Functors?
-- Functor gives us the ability with the list trick, and probably other things, but if we only modify the value,
-- we can Identity out of the trap:
theTrick3 = runIdentity $ ix 2 (\x -> Identity 88) [0..4]
theTrick3' = ix 2 (\x -> Identity (x * 4)) [0..4]

