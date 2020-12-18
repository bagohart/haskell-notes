{-# LANGUAGE RankNTypes #-}

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

-- Iso' s a
-- ^ this is like (s -> a, a -> s)
-- => you can convert between s and a losslessly.

-- Iso s t a b ~ (s -> a, b -> t)
-- even more general, I can change types.
-- does this mean I can "go back" only by changing types before?

-- view someIso :: s -> a
-- view (from someIso) :: (b -> t) -- this looks like "from iso" gives me the other direction?

-- over someIso :: (a -> b) -> (s -> t)
-- over (from someIso) :: (t -> s) -> (b -> a) -- what ?_?

-- and something about type swapping that I don't get...

-- an example is enum.
-- I can go from a character to its integer representation and back, i.e. it's a general iso,
-- where the types change.
-- here, the notion that a is part of s is misleading, as s is also part of a.
-- In other words, s and a are sorta equivalent ?_?

-- We want a "bidirectional function" (a -> f b) <-> (s -> f t)
-- let's try to build a class for that
class Isomorphic k where 
    isomorphic :: (a -> b) -> (b -> a) -> k a b

instance Isomorphic (->) where 
    isomorphic f _ = f
    -- wtf is this

data Isomorphism a b = Isomorphism (a -> b) (b -> a)

instance Isomorphic Isomorphism where 
    isomorphic = Isomorphism

-- this... reverses the iso by... swapping its two functions? o_O
from :: Isomorphic k => Isomorphism a b -> k b a
from (Isomorphism a b) = isomorphic b a

type Iso s t a b = forall k f. (Isomorphic k, Functor f) => k (a -> f b) (s -> f t)
-- now this is like a lens except it also maybe works in reverse ?_?

-- now can I create an iso from functions?
isos :: (s -> a) -> (a -> s) -> (t -> b) -> (b -> t) -> Iso s t a b
isos sa as tb bt = isomorphic
    (\afb s -> bt <$> afb (sa s)) -- :: (a -> f b) -> s -> f t
    (\sft a -> tb <$> sft (as a)) -- :: (s -> f t) -> a -> f b

-- this has 2 problems:
-- isos needs 4 functions instead of 2 (not sure why 2 would suffice, and what the types here even mean)
-- not well composable.
-- I'm not pretending that I got all the details here =/
