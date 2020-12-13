{-# LANGUAGE RankNTypes, TupleSections, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies #-}

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
moreAll3' = view3 (_all' (First (Just 0))) $ (First . Just) <$> [100,600,0,200,0]
moreAll3'' = view3 (_all' [0]) [[0],[0],[1],[2],[0]]
moreAll3''' = view3 (_all' [0]) [[1],[2]]
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

-- the list hackery above seemed to work, so let's try to functionify that out:
-- toListOf = all values
-- recall, this was view:
-- view3 :: Getting s a -> s -> a
-- view3 l = getConst . l Const
-- we want the same thing here, except the value is wrapped in a list:
toListOf1 :: ((a -> Const [a] a) -> s -> Const [a] s) -> s -> [a]
toListOf1 l = getConst . l (Const . pure)
-- which is:
-- toListOf l = getConst . l (\x -> Const [x])

testToListOf1 = toListOf1 (_all' 0) [0,3,1,0]
testToListOf1' = toListOf1 (_all' 0) [3,1]

-- preview: like toListOf, but get only first value
preview1 :: ((a -> Const (First a) a) -> s -> Const (First a) s) -> s -> Maybe a
preview1 l = getFirst . getConst . l (Const . First . Just)

testPreview1 = preview1 (_all' 0) [3,2,1,0]
testPreview2 = preview1 (_all' 0) [3,2,1]

-- has is like just use preview and check if there's anything there, but that would be too easy,
-- so use another Monoid: Any
has1 :: ((a -> Const Any a) -> s -> Const Any s) -> s -> Bool
has1 l = getAny . getConst . l (const (Const (Any True)))

hasPreview1 = has1 (_all' 0) [3,2,1]
hasPreview2 = has1 (_all' 0) [3,2,1,0]

-- now let's simplify the types. Recall:
-- type Getting s a     = (a -> Const a a)  -> s -> Const a s
-- type Setting s t a b = (a -> Identity b) -> s -> Identity t
-- toListOf, preview, has have the same types as Getting, but with them monoidz. => ...
type Getting2 r s a = (a -> Const r a) -> s -> Const r s


view4 ::     Getting2 a         s a -> s -> a
view4 = view3

toListOf2 :: Getting2 [a]       s a -> s -> [a]
toListOf2 = toListOf1

preview2 ::  Getting2 (First a) s a -> s -> Maybe a
preview2 = preview1

has2 ::      Getting2 Any       s a -> s -> Bool
has2 = has1

-- what does the type of Getting2 mean?
-- "if you have a function to get an r from an a, collect all a from s and combine their r's and return them"
-- or keep in mind that Const r a ~~ r, and then its type is not so hard:
-- type Getting2 r s a ~= (a -> r) -> s -> r
-- Analogously,
-- type Setting s t a b ~= (a -> b) -> s -> t

-- why not implement everything in terms of toListOf? Because it would be too easy :)

-- also, toListOf is broken:
-- toListOf1 :: ((a -> Const [a] a) -> s -> Const [a] s) -> s -> [a]
-- toListOf1 l = getConst . l (Const . pure)
-- because this creates a long list of lists, and then appends them together, which can blow up.
-- in case of a simple list, this even works because traverse would append in the right, cheap order:
-- [1] ++ ([2] ++ ...)
-- but we can construct a tree with two list-like branches, and then we get too many big appends.
-- so let's difference list this.

data AppendList a = JustList [a] | Append (AppendList a) (AppendList a)

-- we need to reorder (a ++ b) ++ y ... into a ++ (b ++ (y ++ ...))
doAppends :: AppendList a -> [a]
doAppends (JustList xs) = xs
doAppends (Append (JustList xs) y) = xs ++ doAppends y
doAppends (Append (Append a b) y) = doAppends (Append a (Append b y))

instance Semigroup (AppendList a) where 
    (<>) = Append

instance Monoid (AppendList a) where 
    mempty = JustList []

toListOf3 :: Getting2 (AppendList a) s a -> s -> [a]
toListOf3 l = doAppends . getConst . l (Const . JustList . pure)

-- also there is a weird trick that we can just build a function that appends the list together
-- which kind of achieves the same result:

toListOf4 :: Getting2 (Endo [a]) s a -> s -> [a]
toListOf4 l = (`appEndo` []) . getConst . l (\x -> Const (Endo (x:)))

-- so what this actually does is it builds up a bunch of functions like this:
-- (\xs -> x3 : xs) . (\xs -> x2 : xs) . (\xs -> x1 : xs) $ []
-- = x3 : x2 : x1 : []
-- But wrapped in Endos wrapped in Const.

-- AppLens is called Traversal in lens library.
type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
type Traversal' s a = Traversal s s a a
-- it has even more things.

-- each = focus on every element in a monomorphic container

class Each s t a b | s -> a, t -> b, s b -> t, t a -> s where 
    each :: Traversal s t a b

-- it's just traverse in many cases, e.g. [],Map,Maybe
instance Traversable t => Each (t a) (t b) a b where 
    each = traverse

-- but not Tuples, because (a,) is a Traverseable instance, but not (,) because (,) is not monomorphic:
tupleTraverseTest1 = traverse (\_ -> putStr "? new: " >> readLn :: IO Bool) (1,2) -- enter True here

-- continue: dissecting each
