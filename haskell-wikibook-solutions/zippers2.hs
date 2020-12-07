{-# LANGUAGE InstanceSigs #-}

-- now with real zippers
data Node a = DeadEnd a | Passage a (Node a) | Fork a (Node a) (Node a) deriving (Eq,Show)

-- (0,0) is in the centre
lab1 = Fork (0,2) left right
left = Fork (-2,0) (DeadEnd (-1,0)) (DeadEnd (0,-2))
right = Passage (2,0) $ Fork (1,0) (DeadEnd (0,-1)) (Passage (0,1) (DeadEnd (0,0)))

data Branch a = KeepStraightOn a | TurnLeft a (Node a) | TurnRight a (Node a) deriving (Eq,Show)
type Thread a = [Branch a]

type Zipper a = (Thread a, Node a)

turnRight :: Zipper a -> Maybe (Zipper a)
turnRight (t, Fork x l r) = Just (TurnRight x l : t, r)
turnRight _ = Nothing

turnLeft :: Zipper a -> Maybe (Zipper a)
turnLeft (t, Fork x l r) = Just (TurnLeft x r : t, l)
turnLeft _ = Nothing

keepStraightOn :: Zipper a -> Maybe (Zipper a)
keepStraightOn (t, Passage x n) = Just (KeepStraightOn x : t, n)
keepStraightOn _ = Nothing

back :: Zipper a -> Maybe (Zipper a)
back (KeepStraightOn x : t, n) = Just (t, Passage x n)
back (TurnLeft x r : t, n) = Just (t, Fork x n r)
back (TurnRight x l : t, n) = Just (t, Fork x l n)
back ([],_) = Nothing

-- data Node a = DeadEnd a | Passage a (Node a) | Fork a (Node a) (Node a) deriving (Eq,Show)
get :: Zipper a -> a
get (_, DeadEnd x) = x
get (_, Passage x _) = x
get (_, Fork x _ _) = x

put :: a -> Zipper a -> Zipper a
put x (t, DeadEnd _) = (t, DeadEnd x)
put x (t, Passage _ n) = (t, Passage x n)
put x (t, Fork _ l r) = (t, Fork x l r)

-- Obviously, I could have written those ^
-- in terms of fst, snd and the old get/put functions, but that wasn't the task, so...

update :: (a -> a) -> Zipper a -> Zipper a
update f z = put (f x) z
    where x = get z

-- todo: write game based on labyrinth.
-- game should ask player where she wants to go and tell him if/when he got out.
--
-- 1. choose labyrinth
-- 2. wander to random position in labyrinth
-- 3. let player choose where to go, until player has found labyrinth exit
-- Note that the thread structure seems quite different than what the player gets to look at.
-- To offer the player a choice, I need at every stop to:
-- - inspect the current place
-- - inspect the last place
-- I cannot just offer the player to go 'back' because the solution then is to just go back until the exit is reached.
-- In fact, it is not clear at all what to offer the player:
-- A fork has 3 directions, but how do I call them without giving away the solution?
-- Obviously, this isn't so obvious.

main :: IO ()
main = do
    undefined
