{-# LANGUAGE GADTs #-}

data Expr a = I Int
            | B Bool
            | Add (Expr a) (Expr a)
            | Mul (Expr a) (Expr a)
            | Eq  (Expr a) (Expr a)
            deriving (Show,Eq)

-- Add is defined, but dangerous, because it can create the wrong expressions.
-- So hide it with the normal module system and give the user only this less dangerous thing here:
add :: Expr Int -> Expr Int -> Expr Int
add = Add

mul :: Expr Int -> Expr Int -> Expr Int
mul = Mul

i :: Int -> Expr Int
i = I

b :: Bool -> Expr Bool
b = B

eq :: Expr Bool -> Expr Bool -> Expr Bool
eq = Eq

e1 :: Expr Int
e1 = (i 5 `add` i 1) `mul` i 7

e2 :: Expr Bool
e2 = (b True `eq` b True) `eq` b False

-- e3 :: Expr Int
-- e3 = (b True `eq` i 5)
-- ^ this no longer typechecks

-- eval :: Expr a -> a
-- eval (I n) = n
-- ^ ... we cannot write this function.
-- Although we restricted what the user can see, we have not restricted what the compiler can see here.
-- The relation between the a and the (I n) is lost here.
-- Also, the a could be any type, e.g. Maybe [IO ()] ... u_U
-- What might help is to restrict the return type of the constructors ... ?
-- go to gadt3
