{-# LANGUAGE RankNTypes, ScopedTypeVariables, InstanceSigs #-}

import Data.Profunctor
import Data.Functor.Identity

type Iso s t a b = forall p f. (Profunctor p, Functor f) => p a (f b) -> p s (f t)

-- iso :: (s -> a) -> (b -> t) -> Iso s t a b
-- iso = undefined

-- from :: Iso s t a b -> Iso b a t s
-- from = undefined

-- the thing should be polymorphic enough so the type variables don't matter
-- so you can get a from s, s from a...:
-- _1 :: Lens (a,x) (b,x) a b
-- _1 :: Lens (b,x) (a,x) b a
-- ^ those two are equal

type FBT b t = forall f. Functor f => f b -> f t

-- convert :: FBT b t -> (b -> t)
-- convert fbt = runIdentity
--     . (fbt :: Identity b -> Identity t)
--     . Identity
-- -- ^ fbt works on any Functor, so it works also for Identity.

-- Exercises
-- 1.
-- dimap f g = lmap f . rmap g

-- 2.
newtype Fn a b = Fn (a -> b)

instance Profunctor Fn where 
    lmap :: (a -> b) -> Fn b c -> Fn a c
    lmap g (Fn h) = Fn $ h . g

    rmap :: (b -> c) -> Fn a b -> Fn a c
    rmap g (Fn h) = Fn $ g . h

-- 3.
-- type Iso s t a b = forall p f. (Profunctor p, Functor f) => p a (f b) -> p s (f t)

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso sa bt = \pafb -> dimap sa (fmap bt) pafb 

-- 4.
-- s = input
-- a = output via hidden pipe
-- x = official output (doesn't exist)
data Forget' a s x = Forget' (s -> a)

unForget :: Forget' a s x -> (s -> a)
unForget (Forget' sa) = sa

-- we can convert the input, and change the type of the non-existent output
instance Profunctor (Forget' a) where 
    lmap :: (s -> t) -> Forget' a t c -> Forget' a s c
    lmap g (Forget' ta) = Forget' $ ta . g

    rmap :: (x -> y) -> Forget' a s x -> Forget' a s y
    rmap _ (Forget' sa) = (Forget' sa)

-- 5.
data Tagged' x b = Tagged' b

unTagged :: Tagged' x b -> b
unTagged (Tagged' b) = b

-- constant output, no input
-- so we can change constant output and change type of non-existing input
instance Profunctor Tagged' where 
    lmap :: (y -> x) -> Tagged' x b -> Tagged' y b
    lmap _ (Tagged' b) = Tagged' b

    rmap :: (b -> c) -> Tagged' x b -> Tagged' x c
    rmap g (Tagged' b) = Tagged' $ g b

data Exchange' a b s t = Exchange' (s -> a) (b -> t)

-- 2 independent functions, can append/prepend functions independently
instance Profunctor (Exchange' a b) where 
    lmap :: (s' -> s) -> Exchange' a b s t -> Exchange' a b s' t
    lmap  g (Exchange' sa bt) = Exchange' (sa . g) bt
    
    rmap :: (t -> t') -> Exchange' a b s t -> Exchange' a b s t'
    rmap g (Exchange' sa bt) = Exchange' sa (g . bt)

-- type Iso s t a b = forall p f. (Profunctor p, Functor f) => p a (f b) -> p s (f t)
-- from' :: (p a (f b) -> p s (f t)) -> p t (f s) -> p b (f a)
from' :: Iso s t a b ->                 Iso b a t s
from' g =
    let bt = \b -> runIdentity (unTagged (g (Tagged' (Identity b))))
        sa = unForget (g (Forget' id :: Forget' a a (Identity b))) -- need to add Identity to determine Functor
                        -- which doesn't even exist to satisfy type system
     in \ptfs -> dimap bt (fmap sa) ptfs
-- 1. t should become b
-- 2. (f s) should become (f a)
-- 1. needs bt
-- 2. needs not sure, sa or fafs or something.
-- 1. bt must be hidden in the iso g.
--      If I assume existence of b, put it in a magic box and run g on it, then maaaaybe...
--      ^ ok, actually this was spelled out in "Getting b -> t"
-- 2. If I have (s -> a), then I can just fmap this over the f and get (f s) -> (f a)
--    ^ (The other direction works too if I can choose f = Identity)
--    To get (s -> a), recall the Forget trick: wrapp id into Forget, call the g iso on it,
--    obtain an (s -> a) wrapped into a Forget, take it out, done.

-- ok, that seems to work, and is actually nothing new.
-- Now how to do this using Exchange?
-- I can probably construct the Exchange to contain both bt and sa, but... why?

-- from' :: (p a (f b) -> p s (f t)) -> p t (f s) -> p b (f a)
from'' :: Iso s t a b ->                 Iso b a t s
from'' iso =
    let ex = Exchange' id id
        (Exchange' sa bt) = iso ex :: Exchange' 
     in \ptfs -> undefined
     -- in \ptfs -> dimap undefined (fmap sa) ptfs

-- data Exchange' a b s t = Exchange' (s -> a) (b -> t)

-- ... how to use Exchange in this? It must work for any Profunctor and Functor!?
-- I would have to wrap things in Exchange first...?
--
-- todo: add Identity... somewhere :)
