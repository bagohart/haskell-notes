{-# LANGUAGE RankNTypes, TupleSections #-}

import Data.Bifunctor
import Data.Functor.Compose
import Data.Functor.Const
import Data.Functor.Identity
import Control.Applicative

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a

-- this accesses the first element of a tuple I think
-- _1 :: Functor f => (a -> f b) -> (a, x) -> f (b, x)
_1 :: Lens (a, x) (b ,x) a b
-- _1 g (y,z) = (\b -> (b,z)) <$> g y
-- or, using TypleSections:
_1 g (y,z) = (,z) <$> g y

-- _2 :: Functor f => (a -> f b) -> (x, a) -> f (x, b)
_2 :: Lens (x, a) (x, b) a b
_2 g (z,y) = (z,) <$> g y

-- getter + setter = lens (?) Lens part: (a -> f b) -> s -> f t
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set = \h s -> set s <$> h (get s)
-- lens get set = \h s -> 
--     let the_a                    = get s
--         the_fb                   = h the_a
--         partially_applied_setter = set s
--      in partially_applied_setter <$> the_fb
-- idea:
-- use get, set, h to construct this. uh oh.
-- I need to be able to apply h, so first I need an a.
-- So I have to apply get:
-- get s ~> a
-- Then I can apply h:
-- a ~> f b
-- Now the result should be a f t, and I only get the target t with the set function (s -> b -> t)
-- so I can partially apply set with my s and lift it over f to get an f t
-- This is very magic. hm.
-- take home here: lenses can be created by getter and setter.
-- but then, the data is accessed twice, which is bad.

-- Lens s t a b:     (a -> f b) -> s -> f t
choosing :: Lens s1 t1 a b -> Lens s2 t2 a b -> Lens (Either s1 s2) (Either t1 t2) a b
choosing l1 l2 = \g s -> -- g :: (a -> f b),   s :: (Either s1 s2)
    case s of
      Left s1 -> Left <$> l1 g s1
      Right s2 -> Right <$> l2 g s2
-- Q: why won't this work with bimap?
-- A: because the "Left" needs to be lifted over the f, and bimap works 'under' the "Left"

-- modify target of lens and return result (?_?)
(<%~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
(<%~) l g s = l ((\b -> (b,b)) . g) s
-- idea:
-- to use g, compose it with some Functor that is useful here.
-- The lens l gives us then the corresponding f t, and we need to get b and t out of it.
-- This looks almost like the Storey thing, except it stores b instead of a.
-- But the whole tuple is the functor, the second element needn't be one.
-- so this is like identity with one more element, so using the tuple should be fine.
-- Lens l takes (a -> f b) and s. I have s, I have g :: (a -> b)
-- It gives me f t
-- Construct a Functor f, such that from (f t) I get (b, t)

-- Bonus points for constructing it without lambdas or new functions:
-- The (\b -> (b,b)) seems pretty important here. on the other hand, maybe I can just apply the lens twice,
-- each time in different ways ?_? Let's see
-- 1. Use Identity to get the t.
-- 2. Use Const to get the b.
-- Tuple them together somehow:
(<%~~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
(<%~~) l g s = let t = runIdentity $ l (Identity . g) s
                   b = getConst $ l (Const . g) s
                in (b,t)

-- and while I'm at it, I can probably pointfree this, which probably won't make it more understandable:
(<%~!) :: Lens s t a b -> (a -> b) -> s -> (b, t)
(<%~!) l g = (,) <$> (getConst . l (Const . g)) <*> (runIdentity . l (Identity . g))
-- ok, actually it's not so bad.

-- modify target of lens, return the old value
-- this is almost like the last one
(<<%~) :: Lens s t a b -> (a -> b) -> s -> (a, t)
(<<%~) l g s = let t = runIdentity $ l (Identity . g) s
                   a = getConst $ l Const s
                in (a,t)

-- type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
-- type Lens' s a = Lens s s a a
-- s = t, () = a = b
-- so this accesses a () in the value. the s is any type, so any type contains a () ? o_O
united :: Lens' s ()
united = \g s ->  const s <$> g ()
-- I have an s and a g no a.
-- Since the a is (), I can just run the g on a () and get an (f b).
-- My goal is to get an f t = f s
-- Since I only have an (f b) and there is no obvious relation between the b and the s,
-- I can just throw away the b and replace it with an s.
