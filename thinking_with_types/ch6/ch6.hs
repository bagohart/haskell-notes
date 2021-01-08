{-# LANGUAGE RankNTypes #-}

-- applyToFive1 :: (a -> a) -> Int
-- applyToFive1 f = f 5
-- this does not compile. otherwise, this function would accept any endomorphism,
-- i.e. some String -> String, Int -> Int, Maybe Lol -> Maybe Lol etc.
-- and the a would be chosen by the caller.

-- equivalent to the above, but with explicit forall a:
-- applyToFive2 :: forall a. (a -> a) -> Int
-- applyToFive2 f = f 5

applyToFive3 :: (forall a. a -> a) -> Int
applyToFive3 f = f 5
-- this compiles, but note that this function is stupid: the parameter f can only be the id function!

-- 6.2
-- make polymorphism first-class (and type inference impossible o_O)
-- rank of a function = depth of its polymorphism
-- f1 :: Int -> Int
-- rank(f1) = 0
-- const :: a -> b -> a
-- head :: [a] -> a
-- rank(const) = rank(head) = 1
-- more than rank3 is basically useless.
-- higher rank ~= rank(f) > 1
-- intuition for higher-rank type: a function that takes callbacks
-- rank = how often does control get "handed off"?
-- rank2 = will call polymorphic function for you
-- rank3 = will run a callback which then runs a callback

-- caller of foo determines instantiation of r
-- foo decides what a is.
foo :: forall r. (forall a. a -> r) -> r
foo f = f 5
-- I can call this with e.g. "foo (const 1)"
-- I don't see how I can call this with anything else but the const function =/

-- 6.3
-- forall binds more loosely than ->, so id :: forall a. a -> a is in fact this:
idExplicit :: forall a. (a -> a)
idExplicit = id

-- for every forall on the left side of a function arrow, I gain one rank
-- so this has rank 2 I think ?_?
foo2 :: forall r. ((forall a. (a -> r)) -> r)
foo2 = foo

-- I don't get the definition of rank. What number of arrows? o_O

-- ex. 6.3-i
ex63i :: Int -> (forall a. (a -> a))
ex63i = const id
-- so what rank does that have? 1? 2? according to the "definition", it should have 1, because there is only one forall.

-- ex. 6.3-ii
ex63ii :: forall a b. ((a -> b) -> ((forall c. c -> a) -> b))
ex63ii f g = f (g 1)
-- again, g cannot possibly depend on any value of c because c could be anything, including 1 or ()
-- so the deepest forall is the forall c, and it is to the left of 2 arrows. so is it rank 2?

-- let's test that this actually works...
tex63ii = ex63ii (+1) (const 5)

-- ex. 6.3-iii
-- ex63iii :: forall a b m z. (((forall x. m x -> b (z m x)) -> b (z m a)) -> m a)
-- ex63iii = undefined
-- to the right of the forall x, there are three ->. so does this have rank 3? ...
-- what does this even do? lol.
-- I wouldn't know how to write this. probably some Functor/Monad/MonadTrans constraints missing, too.

-- 6.4
-- Continuation Monad again u_U
