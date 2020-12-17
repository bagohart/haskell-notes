{-# LANGUAGE RankNTypes, TupleSections #-}
-- let's implement all the stuff here

import Data.Bifunctor
import Data.Functor.Compose
import Data.Functor.Const
import Data.Functor.Identity
import Control.Applicative

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a

type Traversal s t a b = forall f. Functor f => (a -> f b) -> s -> f t

type Getting s a = (a -> Const a a) -> s -> Const a s
type Setting s t a b = (a -> Identity b) -> s -> Identity t

type Getter s a = forall r. Getting r s a

over :: Lens s t a b -> ((a -> b) -> s -> t)
over l f = runIdentity . l (Identity . f)

view :: Getting s a -> s -> a
view l = getConst . l Const

set :: Setting s t a b -> b -> s -> t
set l x = runIdentity . l (Identity . (const x))

-- this accesses the first element of a tuple I think
-- _1 :: Functor f => (a -> f b) -> (a, x) -> f (b, x)
_1 :: Lens (a, x) (b ,x) a b
_1 g (y,z) = (,z) <$> g y

-- _2 :: Functor f => (a -> f b) -> (x, a) -> f (x, b)
_2 :: Lens (x, a) (x, b) a b
_2 g (z,y) = (z,) <$> g y

-- getter + setter = lens (?) Lens part: (a -> f b) -> s -> f t
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set = \h s -> set s <$> h (get s)

choosing :: Lens s1 t1 a b -> Lens s2 t2 a b -> Lens (Either s1 s2) (Either t1 t2) a b
choosing l1 l2 = \g s -> -- g :: (a -> f b),   s :: (Either s1 s2)
    case s of
      Left s1 -> Left <$> l1 g s1
      Right s2 -> Right <$> l2 g s2

(<%~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
(<%~) l g s = l ((\b -> (b,b)) . g) s

(<<%~) :: Lens s t a b -> (a -> b) -> s -> (a, t)
(<<%~) l g s = let t = runIdentity $ l (Identity . g) s
                   a = getConst $ l Const s
                in (a,t)

united :: Lens' s ()
united = \g s ->  const s <$> g ()
