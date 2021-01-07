{-# LANGUAGE InstanceSigs #-}

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
