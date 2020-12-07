{-# LANGUAGE GADTs #-}

-- version 1
-- data Expr = I Int | Add Expr Expr | Mul Expr Expr deriving (Show,Eq)

-- -- (5+1)*7
-- e1 :: Expr
-- e1 = (I 5 `Add` I 1) `Mul` I 7

-- eval :: Expr -> Int
-- eval (I i) = i
-- eval (Mul e1 e2) = (eval e1) * (eval e2)
-- eval (Add e1 e2) = (eval e1) + (eval e2)

-- version 2
-- what we want here is to construct expressions which are valid, and only those.
-- The type system should rule out all others. lol?
-- Not sure why we would want both types of expressions in one type, but let's go along with this for the moment.
-- Maybe at the end we can encode the grammar of a whole programming language in the type system. or something.

data Expr = I Int | B Bool | Add Expr Expr | Mul Expr Expr | Eq Expr Expr deriving (Show,Eq)

eval :: Expr -> Maybe (Either Int Bool)
eval (I i) = Just $ Left i
eval (B b) = Just $ Right b
eval (Mul e1 e2) = do
    e1' <- eval e1
    e2' <- eval e2
    op e1' e2'
    where op :: Either Int Bool -> Either Int Bool -> Maybe (Either Int Bool)
          op (Left i1) (Left i2) = Just (Left (i1 * i2))
          op _ _ = Nothing
eval (Add e1 e2) = do
    e1' <- eval e1
    e2' <- eval e2
    op e1' e2'
    where op :: Either Int Bool -> Either Int Bool -> Maybe (Either Int Bool)
          op (Left i1) (Left i2) = Just (Left (i1 + i2))
          op _ _ = Nothing
eval (Eq e1 e2) = do
    e1' <- eval e1
    e2' <- eval e2
    op e1' e2'
    where op :: Either Int Bool -> Either Int Bool -> Maybe (Either Int Bool)
          op (Right b1) (Right b2) = Just (Right (b1 == b2))
          op (Left i1) (Left i2) = Just (Right (i1 == i2))
          op _ _ = Nothing

-- this... works, but is not so nice. What im I doing here o_O

-- (5+1)*7
e1 :: Expr
e1 = (I 5 `Add` I 1) `Mul` I 7

e2 :: Expr
e2 = (B True `Eq` B True) `Eq` B False

e3 :: Expr
e3 = (B True `Eq` I 5)


-- Now let's phantom type this or something. next file.
