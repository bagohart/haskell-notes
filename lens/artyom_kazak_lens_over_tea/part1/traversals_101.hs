{-# LANGUAGE RankNTypes, TupleSections #-}

import Data.Functor.Identity
import Data.Functor.Const
import Data.Monoid

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a

_abs :: Real a => Lens' a a
_abs f n = update <$> f (abs n)
    where
        update x
            | x < 0 = error "_abs: absolute value can't be negative"
            | otherwise = signum n * x

dafuq = over _abs (^2) (-10)
-- I can lens into a real number and... change its absolute value o_O

over :: Lens s t a b -> ((a -> b) -> s -> t)
over l g = runIdentity . l (Identity . g)

set :: Lens s t a b -> b -> s -> t
set l b = over l (const b)

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set = \h s -> set s <$> h (get s)

_all :: Eq a => a -> Lens' [a] a
_all ref = lens get set
    where 
        get = const ref -- I get the a even if it is not contained in the list :)
        set s new = map (\old -> if old == ref then new else old) s -- I set only the same elements

-- how to write this without using lens?
-- _all2 :: (Eq a) => a -> Lens' [a] a
_all2 :: (Eq a, Functor f) => a -> (a -> f a) -> [a] -> f [a]
_all2 ref f list = 
    let -- f_new :: f a
        f_new = f ref
        -- getModifiedList :: a -> [a]
        getModifiedList new = map (\old -> if old == ref then new else old) list
    in getModifiedList <$> f_new


dafuq2 = set (_all 0) (-8) [100,600,0,200,0]
dafuq2' = set (_all2 0) (-8) [100,600,0,200,0]

dafuq3 = (_all 0) (const $ putStr "? new: " >> readLn) [100,600,0,200,0]

-- type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
-- type Lens' s a = Lens s s a a

-- in dafuq3, the user is asked for input only once. With the Functor interface, more seems not possible, either.
-- What happens if we try to use Applicative?

type AppLens s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
type AppLens' s a = AppLens s s a a

_all' :: Eq a => a -> AppLens' [a] a
-- _all' :: (Applicative f, Eq a) => a -> (a -> f a) -> [a] -> f [a]
_all' ref = \g list ->
    let
        -- update :: a -> f a -- updates not-all the things
        update old = if old == ref then g old else pure old -- "f old" looks funny.
        -- contrast with (somewhat comparable maybe) old thing:
        -- getModifiedList new = map (\old -> if old == ref then new else old) list
    in  traverse update list

dafuq4 = (_all' 0) (const $ putStr "? new: " >> readLn) [100,600,0,200,0]

-- problem: we can' use view, over, set with _all' !
-- view :: Lens s t a b -> s -> a
-- ^ so view expects a lens, i.e. something that works on all Functors.
-- recall:
-- type Lens s t a b    = forall f. Functor f       => (a -> f b) -> s -> f t
-- type AppLens s t a b = forall f. Applicative f   => (a -> f b) -> s -> f t
-- if we give view an AppLens, then it rightly complains that this cannot be applied to all those things. hm.

-- view only uses Const, over and set use Identity.
-- So we can make the types more precise and state exactly what we need:

-- view ::  ((a ->       f a) -> s ->       f s) -> s -> a
view2 ::    ((a -> Const a a) -> s -> Const a s) -> s -> a
view2 = undefined

-- over :: ((a ->        f b) -> s ->        f t) -> (a -> b) -> s -> t
over2 ::   ((a -> Identity b) -> s -> Identity t) -> (a -> b) -> s -> t
over2 = undefined

-- set :: ((a ->        f b) -> s ->        f t) -> b -> s -> t
set2 ::   ((a -> Identity b) -> s -> Identity t) -> b -> s -> t
set2 = undefined

-- that was a bit verbose, so let's type synonymize this:
type Getting s a     = (a -> Const a a)  -> s -> Const a s
type Setting s t a b = (a -> Identity b) -> s -> Identity t

-- now we get new signatures for view, over, set:
view3 :: Getting s a -> s -> a
view3 l = getConst . l Const

over3 :: Setting s t a b -> (a -> b) -> s -> t
over3 l g = runIdentity . l (Identity . g)

set3 :: Setting s t a b -> b -> s -> t
set3 l b = over3 l (const b)

-- the new signatures require less: only that the f can be a Const/Identity
-- therefore, the new functions work with MORE lenses!
-- and now I can use them with _all':

moreAll1 = set3 (_all' 0) (-8) [100,600,0,200,0]
moreAll2 = over3 (_all' 0) (+2) [100,600,0,200,0]
-- this doesn't work without Monoids:
moreAll3 = view3 (_all' (Sum 0)) [100,600,0,200,0]
-- whereas this did:
moreAll3Old = view3 (_all 0) [100,600,0,200,0]

-- I need the monoid because...
-- _all' produces an AppLens', i.e. it requires that the used f thingy is an Applicative
-- view3 chooses f = Const, but this is an Applicative only if its first type
-- (if we use Const a a, that's both its types) is a Monoid.
-- Now this means we can only look at the things inside a thing if they are Monoids...?
-- That seems a bit surprising, since it seems to mean that this always looks at more than one thing?
-- If I look at the implementation of _all', this is indeed what happens.
-- So the 
        -- update old = if old == ref then g old else pure old -- "f old" looks funny.
-- removes the (pure old) thing by Const-ing it away, (pure on Const is just Const mempty)
-- and all positive tests are <*>-d together after g-ing them. (this is just Const val)
-- I think this is still somewhat surprising.
-- Not sure if any of this _all/_all' thingy is a good idea o___O
