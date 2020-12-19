{-# LANGUAGE RankNTypes, TypeOperators #-}

-- recall, this is a Lens:
-- Lens s t a b ~ (s -> a, s -> b -> t)
-- Lens s t a b ~ (s -> a, (s, b) -> t)
-- but some incomprehensible stuff about s ~~ s/a
-- and because s ~~ a -> s / a = 1 aka ()
-- because suddenly I can just arithmetic them typez. lol?
--
-- enum as a Lens:
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a
-- type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

-- enum :: Enum a => (a -> f a) -> Int -> f Int
enum :: Enum a => Lens' Int a
enum = \g s -> fromEnum <$> g (toEnum s)
-- ^ this... computes the enum of the int, puts it in a functor and does SomeThing,
-- then possibly computes it back to int

-- consider 2 cases:
-- turn s into a <- I can do this with the last enum definition
-- b into t <- for some reason s does not exist. dafuq?
-- enum f = \_ -> fromEnum <$> f ??? ... ???
-- f can't get input (why ?), so f is constant, therefore 'enum' is constant, too. o_O
--
-- now comes something that I understand:
-- (a -> b), if constant, is isomorphic to b 
data a :-> b = Always b
-- ^ lol.
-- or:
newtype Tagged a b = Tagged { unTagged :: b }
-- note that this looks a lot like Const a b = Const a, but the phantom types are swapped!

enum2 :: (Enum a, Functor f) => Tagged a (f a) -> Tagged Int (f Int)
enum2 (Tagged fa) = Tagged (fromEnum <$> fa)
-- ^ this is just calling enum on an Enum value wrapped in a Functor wrapped in Tagged. ...
-- so I just built s -> a ? or not? hm.

-- but there is this other use case with the constant function, recall:
-- enum = \g s -> fromEnum <$> g (toEnum s)
-- the function should be able to operate on:
-- s
-- const b
-- so... let's build something that holds both:
class IsoFunction p where 
    changeInput :: (s -> a) -> p a b -> p s b
    changeOutput :: (b -> t) -> p a b -> p a t

instance IsoFunction (->) where 
    changeInput f = (. f)
    changeOutput f = (f .)

instance IsoFunction Tagged where 
    changeInput _ = retag
    changeOutput f = fmap f

type Iso s t a b = forall p f. (Functor f, IsoFunction p) => p a (f b) -> p s (f t)
-- ... what does that even mean?

-- unify all the stuff, then maybe find out what all this means...
enum1' (Tagged fa) = Tagged fromEnum <$> fa
enum2' f = fmap fromEnum . f . toEnum
-- ^ this is just writing the previous thing slightly differently.
-- retag :: Tagged x a -> Tagged y a can change the phantom type to anything.

enum1'' fa = fmap fromEnum <$> retag fa -- ...hm?
-- something about changing the types of input and output. uh oh.

retag :: Tagged x y -> Tagged z y
retag = undefined

enum1''' fa = (fmap fromEnum <$>) $ retag      $ fa
enum2''' f  = (fmap fromEnum .)   $ (. toEnum) $ f
-- apparently both apply 2 operations: one for changing input, and one for changing output

enumUnified = changeInput toEnum . changeOutput (fmap fromEnum)

iso :: (s -> a) -> (b -> t) -> iso s t a b
iso sa bt = changeinput sa . changeOutput (fmap bt)

from :: Iso s t a b -> Iso b a t s
from i = iso bt sa
    where 
        sa s = getConst ((i Const) s)
        bt b = runIdentity . unTagged $ i (Tagged (Identity b))
        -- o_O

-- this is a mess. maybe read the next post first, then try this one again.
-- or get some practical applications of Isos first, I'm still not sure what this is even about
