{-# LANGUAGE RankNTypes #-} 

length1 :: [a] -> Int
length1 [] = 0
length1 (x:xs) = 1 + length1 xs

ll = length1

l1 = ll [1,2,3]
l2 = ll ['a','a','a']

length2 :: forall a. [a] -> Int
length2 [] = 0
length2 (x:xs) = 1 + length1 xs

ll' = length2

l3 = ll' [1,2,3]
l4 = ll' ['a','a','a']

-- Pass a function that has, for every type a, an instance (a -> a)
foo :: (forall a.a -> a) -> (Char,Bool)
foo f = (f 'c', f True)

-- This does not work:
-- bar :: (a -> a) -> (Char,Bool)
-- bar f = (f 'c', f True)
--
-- and is equivalent to:
--
-- bar' :: forall a. (a -> a) -> (Char,Bool)
-- bar' f = (f 'c', f True)
--
-- It means:
-- "For every type a, bar excepts a function (a -> a) with fixed type a"
-- but then, this function suddenly has two types, and it does not compile.

