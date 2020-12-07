{-# LANGUAGE InstanceSigs #-}

data FingerList a = Finger a [a] [a] deriving (Show,Eq)

goLeft :: FingerList a -> Maybe (FingerList a)
goLeft (Finger x (y:ys) zs) = Just $ Finger y ys (x:zs)
goLeft _ = Nothing

goRight :: FingerList a -> Maybe (FingerList a)
goRight (Finger x ys (z:zs)) = Just $ Finger z (x:ys) zs
goRight _ = Nothing

get :: FingerList a -> a
get (Finger x _ _) = x

put :: a -> FingerList a -> FingerList a
put x (Finger _ ys zs) = Finger x ys zs

modify :: (a -> a) -> FingerList a -> FingerList a
modify f (Finger x ys zs) = Finger (f x) ys zs

