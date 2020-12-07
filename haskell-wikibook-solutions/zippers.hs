{-# LANGUAGE InstanceSigs #-}

-- this is not zippery yet. this is only the problem.

data Node a = DeadEnd a | Passage a (Node a) | Fork a (Node a) (Node a) deriving (Eq,Show)

get :: Node a -> a
get (DeadEnd x) = x
get (Passage x _) = x
get (Fork x _ _ ) = x

put :: a -> Node a -> Node a
put x (DeadEnd _) = DeadEnd x
put x (Passage _ n) = Passage x n
put x (Fork _ n n') = Fork x n n'

-- (0,0) is in the centre
lab1 = Fork (0,2) left right
left = Fork (-2,0) (DeadEnd (-1,0)) (DeadEnd (0,-2))
right = Passage (2,0) $ Fork (1,0) (DeadEnd (0,-1)) (Passage (0,1) (DeadEnd (0,0)))

-- problem: consumes the labyrinth
turnRight1 :: Node a -> Maybe (Node a)
turnRight1 (Fork _ l r) = Just r
turnRight1 _ = Nothing

data Branch1 = KeepStraightOn | TurnLeft | TurnRight deriving (Eq,Show)

type Thread1 = [Branch1]

turnRight2 :: Thread1 -> Thread1
turnRight2 t = t ++ [TurnRight]

retrieve :: Thread1 -> Node a -> a
retrieve [] n = get n
retrieve (KeepStraightOn:bs) (Passage _ n) = retrieve bs n
retrieve (TurnLeft :bs) (Fork _ l r) = retrieve bs l
retrieve (TurnRight :bs) (Fork _ l r) = retrieve bs r

update :: Thread1 -> (a -> a) -> Node a -> Node a
update [] f n = put (f $ get n) n
update (KeepStraightOn:bs) f (Passage x n) = Passage x (update bs f n)
update (TurnLeft :bs) f (Fork x l r) = Fork x (update bs f l) r
update (TurnRight :bs) f (Fork x l r) = Fork x l (update bs f r)

