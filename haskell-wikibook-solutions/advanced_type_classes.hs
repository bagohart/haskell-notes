{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, InstanceSigs, FunctionalDependencies #-}

-- 1. Multi parameter type classes

-- class Collection1 c where 
--     insert :: c -> e -> c
--     member :: c -> e -> Bool

-- this doesn't compile because where does the e come from and what does it have to do with anything?
-- instance Collection1 [a] where 
--     insert xs x = x:xs
--     member = flip elem

class Eq e => Collection c e where 
    insert :: c -> e -> c
    member :: c -> e -> Bool

instance Eq a => Collection [a] a where 
    insert = flip (:)
    member = flip elem

-- 2. Functional dependencies
-- class Eq e => Collection c e | c -> e where 
--                                  ^   means:
--                              c uniquely identifies e.
-- This makes sense for e.g. [a] -> [a], Hashmap Int -> Int etc.

data Vector = Vector Int Int deriving (Eq,Show)
data Matrix = Matrix Vector Vector deriving (Eq,Show)

-- how can we multiply them? We want
-- (*) :: Matrix -> Matrix -> Matrix
-- (*) :: Matrix -> Vector -> Vector
-- (*) :: Matrix -> Int -> Matrix
-- (*) :: Int -> Matrix -> Matrix
-- 
-- This works, but allows too much:
-- class Mult a b c where 
-- (*) :: a -> b -> c

-- instance Mult Matrix Matrix Matrix where 
--  ...
-- instance Mult Matrix Vector Vector where 
--  ...
-- m1,m2,m3 :: Matrix
-- (m1*m2)*m3 -- type error :) (m1*m2) could have more than one type!
-- (m1*m2) :: Matrix *m3 -- this is ok.
--
-- note that (m1*m2) could have any type after adding e.g. the following instance:
-- instance Mult Matrix Matrix (Maybe Char) where 
--  ...
--
--  Problem: c shouldn't be any type, but be determined by the things that are multiplied:
-- class Mult a b c | a b -> c where 
--
-- Then, the last nonsense instance couldn't exist.
-- I'm not sure though how this would be enforced/checked.
-- Can I just write one instance, and then it's determined?
-- The nature of these dependencies is really unclear.
-- I'm not convinced by these articles.
