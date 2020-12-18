{-# LANGUAGE RankNTypes #-}

import Data.Functor.Contravariant
import Data.Coerce
import Unsafe.Coerce

-- recall
-- type Lens s t a b = forall f. Functor f => 
-- type Traversal s t a b = forall f. Applicative f => 

-- type Getter s a = forall f. (Contravariant f, Functor f) => 
type Fold s a = forall f. (Contravariant f, Applicative f) => (a -> f a) -> s -> f s

-- the type error I get here is different from what the tutorial describes. Why? :
-- Couldn't match representation of type ‘f a’
--                                with that of ‘f (t a)’
--         arising from a use of ‘coerce’
--       NB: We cannot know what roles the parameters to ‘f’ have;
--         we must assume that the role is nominal
-- folded :: Foldable t => Fold (t a) a
-- folded = undefined
-- folded :: (Contravariant f, Applicative f, Foldable t) => (a -> f a) -> t a -> f (t a)
-- folded = \g ta -> coerce $ foldMap g ta
-- idea: Const is an applicative if its elements are Monoids.
-- t is a Foldable, so we can call foldMap on it, and get an f a
-- then coerce type of f a to f (t a) because we can.

-- apparently the problem is that (f a) is not obviously (to the compiler) a Monoid.
-- So let's make this more obvious to the compiler
newtype Folding f a = Folding { getFolding :: f a }

-- this looks weird because I don't have a Monoid instance here,
-- but I just assume that it must sorta be there because the Applicative is there.
instance (Contravariant f, Applicative f) => Semigroup (Folding f a) where 
    (<>) (Folding fx) (Folding fy) = Folding $ fx *> fy

instance (Contravariant f, Applicative f) => Monoid (Folding f a) where 
    mempty = Folding $ coerce1 <$> pure () -- this looks so horrible :)
-- I can't get Data.coerce to run, and I don't currently care what makes it different than discussed here...
-- Probably I'd have to create Coercible instances or something

-- now use this to write folded
folded :: Foldable t => Fold (t a) a
folded = \g ta -> coerce1 $ getFolding $ foldMap (Folding . g) ta

coerce1 :: b -> a
coerce1 = undefined

-- bothF :: (Contravariant f, Applicative f) => (a -> f a) -> (a, a) -> f (a, a)
bothF :: Fold (a, a) a
-- bothF = \g (x,y) -> coerce1 $ getFolding $ (Folding . g) x <> (Folding . g) y 
-- but this should be simpler, so...
bothF = \g (x,y) -> coerce1 $ g x *> g y 
-- ok, that was probably intended

both :: Traversal' (a, a) a
both = \g (x,y) -> (,) <$> g x <*> g y

type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
type Traversal' s a = Traversal s s a a

replicated :: Int -> Fold a a
-- replicated n = \g x -> coerce1 $ traverse g (replicate n x)
-- idea: put x in array n times.
-- map it, then sequence (aka traverse)
-- then change the type
-- actually, this is stupid, because the value is always the same, so it's cheaper to apply g first:
replicated n = \g x -> coerce1 $ sequenceA (replicate n (g x))

-- type Fold s a = forall f. (Contravariant f, Applicative f) => (a -> f a) -> s -> f s

-- foldCombine :: ((a -> f a) -> s -> f s) -> ((a -> f a) -> s -> f s) -> ((a -> f a) -> s -> f s)
foldCombine :: Fold s a -> Fold s a -> Fold s a
foldCombine fa fb = \l s -> fa l s *> fb l s

-- easier to use <> directly:
-- _3 <> _1 <> _2
-- ^ this works because (a -> b) is a Monoid for any (Monoid b) and b is probably Const here or something.
