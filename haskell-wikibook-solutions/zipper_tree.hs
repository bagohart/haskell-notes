{-# LANGUAGE InstanceSigs #-}

data Tree a = Leaf a | Bin (Tree a) (Tree a) deriving (Show,Eq)

t1 :: Tree Int
t1 = Bin (Leaf 1) (Bin (Leaf 2) (Leaf 3))

data Branch a = TurnLeft (Tree a) | TurnRight (Tree a) deriving (Eq,Show)

type Thread a = [Branch a]

type Zipper a = (Thread a, Tree a)

goLeft :: Zipper a -> Maybe (Zipper a)
goLeft (thread, Bin l r) = Just (TurnLeft r : thread, l)
goLeft _ = Nothing

goRight :: Zipper a -> Maybe (Zipper a)
goRight (thread, Bin l r) = Just (TurnRight l : thread, r)
goRight _ = Nothing

back :: Zipper a -> Maybe (Zipper a)
back ([],_) = Nothing
back (TurnLeft node : thread, tree) = Just (thread, Bin tree node)
back (TurnRight node : thread, tree) = Just (thread, Bin node tree)

get :: Zipper a -> Maybe a
get (_, Leaf x) = Just x
get _ = Nothing

put :: a -> Zipper a -> Maybe (Zipper a)
put x (t, Leaf _) = Just (t, Leaf x)
put _ _ = Nothing

update :: (a -> a) -> Zipper a -> Maybe (Zipper a)
update f (t, Leaf x) = Just (t, Leaf $ f x)
update _ _ = Nothing
