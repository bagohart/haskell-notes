{-# LANGUAGE GADTs #-}

data Expr a where 
    I :: Int -> Expr Int 
    B :: Bool -> Expr Bool
    Add :: Expr Int -> Expr Int -> Expr Int
    Mul :: Expr Int -> Expr Int -> Expr Int
    Eq ::  Eq a => Expr a -> Expr a -> Expr Bool
-- how to derive the things? is it even possible with GADTs at all?

-- and now we can eval all the things?
eval :: Expr a -> a
eval (I n) = n -- the compiler now knows that this means that the type was Expr Int. magic!
eval (B b) = b
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Eq e1 e2) = eval e1 == eval e2
-- ... this seemed almost too easy?

data Bar where 
    BarNone :: Bar

data Foo where 
    MkFoo :: Foo
    -- MkFoo :: Bar
