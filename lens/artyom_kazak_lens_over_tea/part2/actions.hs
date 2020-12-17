{-# LANGUAGE RankNTypes, TupleSections #-}

import Data.Bifunctor
import Data.Functor.Contravariant
import Data.Functor.Compose
import Data.Functor.Const
import Data.Functor.Identity
import Control.Applicative
import Control.Monad


-- type Getter   s a = s ->   a
-- type Action m s a = s -> m a
--
-- but actually more like
-- type Getter   s a = forall r. (a ->   r) -> s ->   r
-- type Action m s a = forall r. (a -> m r) -> s -> m r
--
-- but actually with Const to use Lens
-- type Getter   s a = forall r. (a -> Const r a) -> s -> Const r s
-- type Action m s a = forall r. (a -> Const (m r) a) -> s -> Const (m r) a
--
-- now let's namify this
type Effect m r = Const (m r)

type Action m s a = forall r. (a -> Effect m r a) -> s -> Effect m r s

-- Recall the symmetry:
-- type Getting r s a = (a -> Const r a) -> s -> Const r s
-- type Getter s a = forall r. Getting r s a
-- now let's do the same for Action:
type Acting m r s a = (a -> Effect m r a) -> s -> Effect m r s
-- note that
-- type Acting m r s a = (a -> Const (m r) a) -> s -> Const (m r) s
-- and therefore
-- type Acting m r s a = Getting (m r) s a
-- but Action:
-- type Action m s a = forall r. (a -> Effect m r a) -> s -> Effect m r s
-- type Action m s a = forall r. (a -> Const (m r) a) -> s -> Const (m r) s
-- cannot be represented as Getter because the r != (m r) I guess
-- therefore we cannot use to here, and I have already forgotten what this does o_O

-- from the whole thing read an element with monad stuff stuffed around.
-- turn this into something which reads the inner thing and supplies the outer thing and then get something out of it
-- ... o_O
-- I can fire g on the s because why not. I get an m a.
-- I can lift the h over the m and get an m (Const (m r) a)
-- I can remove the inner Const and get   m (m r)
-- I can join this and get                m r
-- I can put it in a Const and get        Const (m r) ?
-- The ? can be anything, so              Const (m r) s
--                        g                         h                s
-- act :: Monad m => (s -> m a) -> forall r. (a -> Const (m r) a) -> s -> Const (m r) s
act :: Monad m => (s -> m a) -> Action m s a
act g = \h s -> Const $ do
    a <- g s
    (getConst . h) $ a
-- the Action needs a function to work on a part of the structure
-- It is built by supplying a function that works on the whole structure.
-- Not sure where this is going.

-- this is view (flipped)
-- (^.) :: s -> Getting a s a -> a
-- (^.) = undefined

-- so this... looks at a thing in a structure, but the result is in monad-thingy?
-- (^!) :: s -> ((a -> Const (m a) a) -> s -> Const (m a) s) -> m a
-- (^!) :: s -> Getting (m a) s a -> m a
(^!) :: Monad m => s -> Acting m a s a -> m a -- Applicative m is actually sufficient to implement this?
(^!) s acting = getConst $ acting (Const . pure) s
-- I have an s
-- I have a function that expects this s, and a ??? and gives me a Const (m a) s which is basically a (m a),
-- which is what I need. So I have to give it the ???
-- ??? = puts thing in monad and wraps it in const. so probably this is just puring the part of the whole thing...

infixr 8 ^!

-- recall:
-- type Acting m r s a = (a -> Effect m r a) -> s -> Effect m r s
-- type Acting m r s a = (a -> Const (m r) a) -> s -> Const (m r) s
-- and therefore
-- type Acting m r s a = Getting (m r) s a
-- type Getting r s a = (a -> Const r a) -> s -> Const r s

perform :: Monad m => Acting m a s a -> s -> m a
perform = flip (^!)

coolStuff :: IO ()
coolStuff = "Hello, world!" ^! act putStrLn

each = traverse

-- this shouldn't work according to tutorial, but now this monoid instance actually exists...
moreStuff = ["a","b","c"] ^! each . act putStrLn 

-- to work around this restriction with monoids, more magic now...
-- newtype Effect' m r a = Effect' { getEffect' :: m r }
-- recall old: type Effect' m r = Const (m r)
newtype Effect' m r a = Effect' { getEffect' :: m r }

instance Functor (Effect' m r) where
  fmap _ (Effect' m) = Effect' m

instance (Monad m, Monoid r) => Applicative (Effect' m r) where
  pure _ = Effect' (return mempty)
  Effect' ma <*> Effect' mb = Effect' (liftM2 mappend ma mb)
  
-- So... we have reimplemented Const, but with the built-in monad. ... and?
-- and with the monad-as-monoid thing, why?
-- probably just because now we don't have to newtype monads with monoids all the time?

-- adapt the things...
type Action' m s a = forall r. (a -> Effect' m r a) -> s -> Effect' m r s
type Acting' m r s a = (a -> Effect' m r a) -> s -> Effect' m r s

act' :: Monad m => (s -> m a) -> Action' m s a
act' g = \h s -> Effect' $ do
    a <- g s
    (getEffect' . h) $ a

(^!!) :: Monad m => s -> Acting' m a s a -> m a 
(^!!) s acting = getEffect' $ acting (Effect' . pure) s

infixr 8 ^!!

-- try again...
moreStuff2 = ["a","b","c"] ^!! each' . act' putStrLn 
each' = traverse

-- and now try to compose actions with to things.
-- turns out, then we try to compose Effects with Consts and that doesn't fly.
-- So we need to do even more type level magic. Let's see...
-- there is Contravariant, which is like Functor but reverse.
-- We can combine Contravariant with Functor and get something which is guaranteed to have no value:
-- This is proven by implementing the following:
-- contramap :: (a -> b) -> f b -> f a
-- fmap (a -> b) -> f a -> f b
coerce :: (Functor f, Contravariant f) => f a -> f b
-- coerce fa = fmap undefined fa
-- coerce fa = contramap undefined fa
-- ^ this would probably be cheating and also pointless...
coerce fa = contramap (const ()) (fmap (const ()) fa)
-- idea:
-- f a ~> f () ~> f b
-- we remove the value first, and then ... sorta again o_O
-- since contramap can just invent any a we need, and const can also assume any value we need, this actually works

