{-# LANGUAGE RankNTypes, ScopedTypeVariables, InstanceSigs, FlexibleInstances, UndecidableInstances #-}

import Data.Profunctor
import Data.Functor.Identity
import Data.Maybe

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
    let ex = Exchange' id Identity
        (Exchange' sa ibt) = iso ex
        bt = runIdentity . ibt
     in \ptfs -> dimap bt (fmap sa) ptfs

-- the Identity thing could also be stated like this:
from''' :: Iso s t a b ->                 Iso b a t s
from''' iso =
    let ex = Exchange' id fid
        (Exchange' sa fbt) = iso ex
        bt = runIdentity . fbt . Identity
     in \ptfs -> dimap bt (fmap sa) ptfs

fid :: Identity a -> Identity a
fid = id

-- affine traversal: traversal that always extracts Nothing or a single value.
-- observe: Lens uses Functor, accesses 1 element
-- Traversal uses Applicative, can access many elements
-- Traversal1 uses Apply, must access at least 1 element
-- new: use Pointed, which is like Applicative without <*>, i.e. only pure and fmappable

-- type Traversal01 s t a b = forall f. (Functor f, Pointed f) => (a -> f b) -> s -> f t
-- don't have Pointed in scope :'(

-- recap lens thingys
-- With a Lens you have:
-- get :: s -> a
-- put :: s -> a -> s
-- you can combine those two like this:
-- lens :: s -> (a, a -> s)
-- if you put in an s, you get (a -> s) which is like "s with an a-shaped hole", and we also have this a,
-- so we can construct back the s again.
-- Therefore, s is isomorphic to (a, a -> s). (Unless your lens laws are broken)
-- It's like e just deconstructed a product type...
-- Now the prism deconstructs a sum type. Really?
-- We get:
-- get :: s -> Maybe a
-- put :: a -> s
-- Look at a sum type:
-- let's pretend those are all natural numbers, i.e. > 0
data Natural = Natural Integer deriving (Eq, Show)

data Integer' = Positive Natural | Zero | Negative Natural  deriving (Eq, Show)
-- which is equivalent to
data Integer'' = Either Natural (Either Natural ()) deriving (Eq, Show)

prismI :: (Integer -> Maybe Natural, Natural -> Integer)
prismI = (toNatural, toInteger)
    where 
        toNatural n = if n > 0 then Just (Natural n) else Nothing
        toInteger (Natural i) = i

-- prism is like an Isomorphism, but it can fail in one direction.
-- also, the get thing is like an affine traversal ?_? ... so?

-- simple prism: no type change. not-simple prism: can change tye.
-- this is a simple prism:
-- natural :: Prism' Integer Natural
-- natural = undefined

getNatural :: Integer -> Maybe Natural
getNatural = undefined
putNatural :: Natural -> Integer
putNatural = undefined

-- we can use getNatural and putNatural to write modifyNatural
modifyNatural :: (Natural -> Natural) -> Integer -> Integer
modifyNatural f int = case getNatural int of
                        Nothing -> int
                        Just nat -> putNatural (f nat)

-- _Left :: Prism (Either a c) (Either b c) a b

modifyLeft :: (a -> b) -> Either a c -> Either b c
modifyLeft g eac = case getLeft eac of
                     Nothing -> putRight (fromJust . getRight $ (eac)) -- change the type
                     Just x -> putLeft $ g x

-- can we write this ^ with get* and put*? Why not?
-- so we have:
getLeft :: Either a b -> Maybe a
getLeft = undefined

putLeft :: a -> Either a b
putLeft = Left

getRight :: Either a b -> Maybe b
getRight = undefined

putRight :: a -> Either z a
putRight = Right

-- so we COULD write it, but it required putRight/fromJust/getRight to coerce the (Left-) type in case of Right
-- we could also change getLeft:
getLeft2 :: Either a c -> Either (Either b c) a
getLeft2 = undefined

modifyLeft2 :: (a -> b) -> Either a c -> Either b c
modifyLeft2 g eac = case getLeft2 eac of
                      Left t -> t
                      Right a -> putLeft $ g a

-- everything works o_O now let's generalise. uh oh.

type Prism' s a = Prism s s a a

-- prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
-- prism = undefined

prism' :: (a -> s) -> (s -> Maybe a) -> Prism' s a
prism' = undefined

-- this won't fly with the usual types because it needs an a, but the Lens thing needs an s!
-- review :: Prism' s a -> a -> s
-- review = undefined

-- and suddenly we need Profunctors. um. what?
-- and here's a new scary box, which we call Bypass.
-- it's a simple a -> b, but it can also bypass this (a->b) box entirely.
-- build a new class for the bypass thing.
class Bypass p where
    bypass :: (s -> Either a t) -> p a t -> p s t

type Prism s t a b = forall p f. (Profunctor p, Bypass p, Applicative f) => p a (f b) -> p s (f t)
-- Applicative provides pure to turn things into f in case of bypass or something.

-- data Tagged' x b = Tagged' b
instance Bypass (Tagged') where 
    bypass :: (s -> Either a t) -> Tagged' a t -> Tagged' s t
    bypass _ (Tagged' t) = (Tagged' t)

-- we could (apparently?) write Prisms already, but this is supposed to be a better way:
class Profunctor p => Choice' p where 
    left''  :: p a b -> p (Either a c) (Either b c)
    left'' pab = dimap swap swap (right'' pab)
        where swap :: Either a b -> Either b a
              swap (Left x) = Right x
              swap (Right y) = Left y
        -- exercise^
        -- idea:
        -- use right' to get
        -- right' pab :: p (Either c a) (Either c b)
        -- turn input (Either a c) ~> (Either c a)
        -- turn output (Either c b) ~> (Either b c)
    right'' :: p a b -> p (Either c a) (Either c b)

-- the next two exercises seem to be about how Choice and Bypass are quite similar
instance Choice' p => Bypass p where 
    bypass :: (s -> Either a t) -> p a t -> p s t
    bypass g pat = dimap g removeEither (left'' pat) 
        where removeEither :: Either a a -> a -- turns out this is easier written as "either id id"
              removeEither (Left x) = x
              removeEither (Right x) = x
        -- we have lmap,rmap,left'',right''. now write bypass.
        -- idea:
        -- we want to lmap the g to the pat, but pat has the wrong type, so maybe we can left'' it or something.
        -- left'' pat :: p (Either a c) (Either t c)
        -- ^ this computes a to t, or just carries c along, which then would be t.
        -- ^ as a result, we have t or t wrapped in an Either, which we can then rmap to t

instance (Profunctor p, Bypass p) => Choice' p where 
    right'' :: p a b -> p (Either c a) (Either c b)
    right'' pab = bypass conv (rmap Right pab)
        where conv :: Either c a -> Either a (Either c b) -- code golfable into "either (Right . Left) Left"
              conv (Right a) = Left a
              conv (Left c) = Right (Left c)
    -- idea:
    -- I can lmap, rmap and bypass, also I have the pab.
    -- I can use bypass on pab, and get psb, if I have (s -> Either a b)
    -- bypass requires that the end result is the same with or without going through the box.
    -- I can probably (only) accomplish that using Either, so the first step might be rmapping "Right" on it:
    -- rmap Right pab :: p a (Either c b)
    -- which is almost what I want, except I need to lmap an "(Either c a) -> a" somehow.
    -- Erroring out on left is possible but bogus, so in case of Left, I have to bypass this somehow.
    -- bypass gives me a "p s t", and I want "p (Either c a) (Either c b)", therefore
    -- s ~ Either c a
    -- t ~ Either c b
    -- Also, I have p a t ~ p a (Either c b)
    -- So I need (Either c a) -> (Either a (Either c b))
    -- I can map the Right a to a Left a, that is easy,
    -- and I can map a Left c to a Right (Left c), so that looks manageable.
    -- Not sure if I confused myself somewhere here though :)
    -- ...
    -- ok, that seemed to actually work. I mean, it compiles. Lol.
    -- the solutions also seem identical except they're pointfree and briefer.

-- data Tagged' x b = Tagged' b
type AReview t b = Tagged' b (Identity b) -> Tagged' t (Identity t)
-- ^ this is just (b -> t), but wrapped in noise.

review :: AReview t b -> b -> t
review r = runIdentity . unTagged . r . Tagged' . Identity

-- type Prism s t a b = forall p f. (Profunctor p, Bypass p, Applicative f) => p a (f b) -> p s (f t)
-- so, it seems that AReview is a type of prism, and review thus uses a prism to convert b into t.
-- This is supposed to be the same as for Isomorphisms.
-- But I'm not sure that I have seen this before.

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism bt sEta = \paFb ->
    let -- paft :: forall p f. (Profunctor p, Bypass p, Applicative f) => p a (f t)
        paft = (rmap (fmap bt) paFb)
        sEta' = either (Right . pure) Left . sEta
     in bypass sEta' paft
    -- idea: this is supposed to be easy using bypass.
    -- p s t ~ p s (f t0) => t ~ f t0
    -- get a "p a (f t0)" for bypass
    -- I have a "p a (f b)" already, but I can "rmap (fmap bt)" this and get "p a (f t0)"
    -- I need a (s -> Either a t) which is "s -> Either a (f t0)"
    -- and I can probably construct this from sEta, swap und pure... ?_?
    -- ... ok, it seems to have worked, but I don't see how this is "trivial"
    -- let's try again with right''

    -- bypass :: (s -> Either a t) -> p a t -> p s t

prism2 :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism2 bt sEta = dimap sEta (either pure (fmap bt)) . right''
    -- idea: this is supposed to be harder than using bypass.
    -- p is Choice', because it is Profunctor and Bypass.
    -- right'' can make the left and right parts into Eithers.
    -- right'' paFb :: p (Either c a) (Either c (f b))
    -- If I lmap sEta on it, I require c ~ t:
    -- right'' paFb :: p (Either t a) (Either t (f b))
    -- lmapping sEta on this makes it to
    -- p s (Either t (f b))
    -- Then I can use bt to go to
    -- p s (Either t (f t))
    -- And I can then pure this and either it into "f t"
    -- Actually, this way seems more straightforward than the bypass thing.
    -- Or maybe I'm just getting used to it already. u_U
    -- ... ok, I in fact implemented exactly the sample solution.
    -- So what would have been the trivial solution for bypass ?_?

    -- right'' :: p a b -> p (Either c a) (Either c b)
    -- type Prism s t a b = forall p f. (Profunctor p, Bypass p, Applicative f) => p a (f b) -> p s (f t)

-- decompose prisms back to the functions with which it was created:
data Market a b s t = Market (b -> t) (s -> Either t a)

instance Functor (Market a b s) where 
    fmap :: (t -> u) -> Market a b s t -> Market a b s u 
    fmap g (Market bt sEta) = Market (g . bt) ((either (Left . g) Right) . sEta)

instance Profunctor (Market a b) where 
    lmap :: (s -> s2) -> Market a b s2 t -> Market a b s t
    lmap g (Market bt s2Eta) = Market bt (s2Eta . g)

    rmap :: (t -> u) -> Market a b s t -> Market a b s u 
    rmap = fmap

-- this collides with the things. wtf
-- instance Choice' (Market a b) where 
--     --                           Market (b -> Either c t) (Either c s -> Either (Either c t) a)
--     right'' :: Market a b s t -> Market a b (Either c s) (Either c t)
--     right'' (Market bt sEta) = Market (Right . bt) (either (Left . Left) ((either (Left . Right) (Right)) . sEta))
--  ^ this is too wild for even the sample solutions, which breaks down the nested eithers in a case distinction.

-- type Prism s t a b = forall p f. (Profunctor p, Bypass p, Applicative f) => p a (f b) -> p s (f t)
unPrism :: Prism s t a b -> (b -> t, s -> Either t a)
unPrism p = 
    let -- bft :: b -> Identity t
        -- sEtfa :: s -> Either (Identity t) a
        Market bft sEtfa = p (Market Identity Right) -- put in Market a b a b
        bt = runIdentity . bft
        sEta = either (Left . runIdentity) Right . sEtfa
     in (bt, sEta)
    -- idea: use Market to put functions into the prism that do nothing. result of prism is new market that contains functions, maybe.
    -- get functions out and remove all the wrapping pain.

-- Lens defines withPrism instead, which looks like unPrism, but in CPS o_O
-- also it uses APrism which is Prism with concrete profunctors and Applicatives (Market and Identity)
-- which means it has less requirements, since Prism works on all the Profunctors and Applicatives.

type Prism1 s t a b = forall p f. (Choice' p, Functor f) => p a (f b) -> p s (f t)

-- prism1ToIso :: Prism1 s t a b -> Iso s t a b
-- prism1ToIso = undefined
-- sa = ...
-- ^ I lost iso somewhere...
getSa :: Prism1 s t a b -> (s -> a)
getSa p = unForget $ p (Forget' id :: Forget' a a (Identity b))
-- ^ I did this before... there seems to be nothing new here?

