{-# LANGUAGE InstanceSigs #-}

-- Basically, this is two lists.

data List a = Cons a (List a) | Nil deriving (Show,Eq)

l1 :: List Int
l1 = Cons 1 (Cons 2 (Cons 3 Nil))

data Branch a = Forward a deriving (Eq,Show)

type Thread a = [Branch a]

type Zipper a = (Thread a, List a)

goForward :: Zipper a -> Maybe (Zipper a)
goForward (thread, Cons x xs) = Just (Forward x : thread, xs)
goForward _ = Nothing

back :: Zipper a -> Maybe (Zipper a)
back ([],_) = Nothing
back (Forward x : thread, xs) = Just (thread, Cons x xs)

get :: Zipper a -> Maybe a
get (_, Cons x _) = Just x
get _ = Nothing

put :: a -> Zipper a -> Maybe (Zipper a)
put x (t, Cons _ xs) = Just (t, Cons x xs)
put _ _ = Nothing

update :: (a -> a) -> Zipper a -> Maybe (Zipper a)
update f (t, Cons x xs) = Just (t, Cons (f x) xs)
update _ _ = Nothing

