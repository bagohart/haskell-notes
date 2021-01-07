{-# LANGUAGE InstanceSigs, DeriveFunctor, ScopedTypeVariables #-}

newtype T1 a = T1 (Int -> a)

instance Functor T1 where 
    fmap :: (a -> b) -> T1 a -> T1 b
    fmap g (T1 h) = T1 $ g . h
-- ^ this is just standard Reader monad stuff.

-- T2 has a Contravariant, this is the standard example. No Functor here.
newtype T2 a = T2 (a -> Int)

newtype T3 a = T3 (a -> a)
-- I could make this into a -> b, but need b -> b, so I also need a (b -> a) function...

newtype T4 a = T4 ((Int -> a) -> Int)
-- this seems impossible: I have a function that expects an (Int -> a).
-- I would need a (b -> a), so I can build a function ((Int -> b) -> Int) which converts its argument
-- before reusing the inner function.

newtype T5 a = T5 ((a -> Int) -> Int)
-- This works, but looks pretty magic. The thing to notice is that I have both ((a -> Int) -> Int) and (b -> Int) ...
-- I have something that expects (a -> Int)
-- I can turn a into b.
-- I have a (b -> Int)
-- I can use that to get a (a -> Int) and feed that to the original function

instance Functor T5 where 
    fmap :: (a -> b) -> T5 a -> T5 b
    fmap ab (T5 aii) = T5 $ \bi -> aii $ bi . ab

-- magic variance rules:
-- covariant: T a is covariant with respect to its type variable a if a appears solely in positive position
-- contravariant: T a is covariant with respect to its type variable a if a appears solely in negative position
-- invariant: T a is covariant with respect to its type variable a if a appears both in positive and negative position
--
-- all types have a canonical representation expressed with (,), Either, (->). Note:
-- Either a b, (a,b): both positive
-- a -> b: a negative, b positive
-- The signs follow the law of multiplication, i.e. - * - = +, - * + = + * - = -, + * + = +
-- Consider (a,Bool) -> Int
-- ^ a in (a,Bool) is positive, (a,Bool) in (a,Bool) -> Int is negative => + * - = - => a is in negative position
-- Apply this on the types T1 - T5:
--
-- newtype T1 a = T1 (Int -> a)
--                           +
-- => covariant
-- newtype T2 a = T2 (a -> Int)
--                    -
-- => contravariant
-- newtype T3 a = T3 (a -> a)
--                    -    +
-- => invariant
--
-- newtype T4 a = T4 ((Int -> a) -> Int)
--                            +
--                             -
--                             + * - = -
-- => contravariant
-- newtype T5 a = T5 ((a -> Int) -> Int)
--                     -
--                             -
--                      - * - = +
-- => covariant
--
-- Other interpretation:
-- positive position = produced/owned
-- example:
--      (Int, a)
--      Either Int a
--      Int -> a
-- negative position = consumed
-- example:
--      a -> Int
--
-- covariant in 2 arguments: bifunctor
-- Example: Either, (,)
-- contravariant in its first, covariant in its second argument: profunctor
-- Example: (->)

-- ... if all of this is correct, I should be able to predict and construct even more Functor instances in a similar way:
newtype T6 a = T6 ((a -> Int) -> a)
-- this is a combination of T1 and T5...
instance Functor T6 where 
    fmap :: (a -> b) -> T6 a -> T6 b
    fmap ab (T6 aia) = T6 $ \bi -> ab (aia (bi . ab))

newtype T7 a = T7 (((Int -> a) -> Int) -> Int)
-- this is like T4, but nested in yet another function, so it should be like + * - * - = +
-- ((Int -> a) -> Int) -> Int
--          +
--                   -
--                           -
-- automatic deriving works, so this should be possible manually...
runT7 :: T7 a -> (((Int -> a) -> Int) -> Int)
runT7 (T7 x) = x

instance Functor T7 where 
    fmap :: (a -> b) -> T7 a -> T7 b
    fmap ab (T7 iaii) = T7 $ \ibi -> iaii $ (\ia -> ibi $ ab . ia)
        -- I need to produce an Int.
        -- I need to apply either iaii or ibi to produce this Int.
        -- Let's try aiaa first:      iaii :: ((Int -> a) -> Int) -> Int
        -- So iaii needs an ((Int -> a) -> Int), then it can produce an Int for us.
        -- And what I have is an ibi :: (Int -> b) -> Int which hopefully matches
        -- So I'll need to turn the ibi into an iai using ab:
        -- ibi :: (Int -> b) -> Int
        -- + ab :: a -> b
        -- ???
        -- ~> iai :: (Int -> a) -> Int
        -- ??? = \ia -> ibi $ ab . ia
        --
        -- ... ok, so that worked, though I won't pretend to be able to hold all of that in my head at once :)

-- since it was so fun, let's level up this again...

-- this is like T7, except now it's becoming ridiculous...
newtype T8 a = T8 (((((Int -> a) -> Int) -> Int) -> Int) -> Int) -- deriving Functor

instance Functor T8 where 
    fmap :: (a -> b) -> T8 a -> T8 b
    fmap ab (T8 iaiiii) = T8 $ \ibiii -> iaiiii $ (\iaii -> ibiii (runT7 (fmap ab (T7 iaii))))
-- this is too convoluted to keep it in my hand but just looking at the types it becomes obvious
-- that this reuses T7...

-- previous attempt that doesn't work because a !~ a1 or something, even with ScopedTypeVariables o_O
-- instance Functor T8 where 
--     fmap :: (a -> b) -> T8 a -> T8 b
--     fmap ab (T8 iaiiii) = T8 $ \ibiii -> 
--         let lol_iaiii :: (((Int -> a) -> Int) -> Int) -> Int
--             lol_iaiii = \iaii -> ibiii (runT7 (fmap ab (T7 iaii)))
--             -- ibiii (lol_ibii
--          in iaiiii $ lol_iaiii
    -- ?1 :: iaiii
    -- => from ibiii and ab create iaiii somehow
    -- ?1 = (\iaii

    -- fmap ab (T7 iaii) = T7 $ \ibi -> iaii $ (\ia -> ibi $ ab . ia)
