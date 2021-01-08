{-# LANGUAGE GADTs #-}
-- ^ GADT activates type equality constraints and generalized algebraic datatypes
{-# LANGUAGE ConstraintKinds, DataKinds, GADTs, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
-- ^ things for 5.3
{-# LANGUAGE FlexibleInstances #-}
-- ^ apparently also needed for 5.3, says the compiler

import Data.Kind (Constraint, Type)

five :: Int
five = 5

five_ :: (a ~ Int) => a
five_ = 5

-- ~ is a type equality. it is an equivalence relation, i.e. reflexive, symmetric, transitive.

-- 5.2
-- type safe syntax tree: canonical example for GADT:
data Expr a where 
    LitInt :: Int -> Expr Int
    LitBool :: Bool -> Expr Bool
    Add :: Expr Int -> Expr Int -> Expr Int
    Not :: Expr Bool -> Expr Bool
    If :: Expr Bool -> Expr a -> Expr a -> Expr a

evalExpr :: Expr a -> a
evalExpr (LitInt i) = i
evalExpr (LitBool b) = b
evalExpr (Add x y) = evalExpr x + evalExpr y
evalExpr (Not x) = not $ evalExpr x
evalExpr (If b x y) =
    if evalExpr b
       then evalExpr x
       else evalExpr y

t1 :: Int
t1 = evalExpr $ Add (LitInt 5) (Add (LitInt 7) (LitInt 3))

-- t2 :: Int
-- t2 = evalExpr $ Add (LitInt 5) (LitBool True)
-- ^ the compiler correctly rejects this.

-- if we remove the syntactic sugar, type equalities remain:
data Expr_ a
    = (a ~ Int) => LitInt_ Int
    | (a ~ Bool) => LitBool_ Bool
    | (a ~ Int) => Add_ (Expr_ Int) (Expr_ Int)
    | (a ~ Bool) => Not_ (Expr_ Bool)
    | If_ (Expr_ Bool) (Expr_ a) (Expr_ a)
  -- ^ the type equality constraint only comes into scope when pattern matching on a data constructor
  -- Therefore a function Expr a -> a can return Int or Bool when matching on LitInt or LitBool
  -- hm.
  -- this type equality thing here is apparently the real novel thing of gadts, the rest is syntactic sugar...

-- 5.3
-- Let's build a magic list

data HList (ts :: [Type]) where 
    HNil :: HList '[]
    (:#) :: t -> HList ts -> HList (t ': ts)

infixr 5 :#

l1 = 1 :# Just "lol" :# HNil

hLength :: HList ts -> Int
hLength HNil = 0
hLength (_ :# ts) = 1 + hLength ts

-- this head function is total!!!
hHead :: HList (t ': ts) -> t
hHead (t :# _) = t

showBool :: HList '[_1, Bool, _2] -> String
showBool (_ :# b :# _ :# HNil) = show b

l2 = 1 :# True :# 3 :# HNil
l2' = 1 :# False :# 3 :# HNil

-- this works, but see below...
-- instance Eq (HList '[]) where 
--     HNil == HNil = True

-- instance (Eq t, Eq (HList ts)) => Eq (HList (t ': ts)) where 
--     (a :# as) == (b :# bs) = a == b && as == bs

teq1 = HNil == HNil
teq2 = l2 == l2'

-- exercise 5.3-i
-- this works, but redefined later
-- instance Ord (HList '[]) where 
--     HNil `compare` HNil = EQ

-- instance (Ord t, Ord (HList ts)) => Ord (HList (t ': ts)) where 
--     (a :# as) `compare` (b :# bs) = if a == b then as `compare` bs else a `compare` b

-- exercise 5.3-ii
-- instance Show (HList '[]) where 
--     show HNil = "[]"

-- instance (Show t, Show (HList ts)) => Show (HList (t ': ts)) where 
--     show (x :# xs) = "[" ++ show x ++ 
--         if hLength xs > 0 then ", " ++ tail (show xs) else "]"
    -- this is... a bit stupid, but it works t_t

-- for Eq, we had to write two instances. This seems stupid.
-- Fix: use closed type family to fold ts into a big CONSTRAINT. lol. ?
type family AllEq (ts :: [Type]) :: Constraint where 
    AllEq '[] = () -- this is the Unit constraint. I guess this one is always satisfied...
    AllEq (t ': ts) = (Eq t, AllEq ts) -- this is a Constraint tuple. this seems to be a thing.

-- in ghci, use :set -XDataKinds and :kind! AllEq '[Int, Bool] to see the result: a nested tuple constraint.

-- note that there was nothing special about Eq in AllEq. Let's generalize this...
type family All (c :: Type -> Constraint)
                (ts :: [Type]) :: Constraint where 
    All c '[] = ()
    All c (t ': ts) = (c t, All c ts)

instance All Eq ts => Eq (HList ts) where 
    HNil == HNil = True
    (a :# as) == (b :# bs) = a == b && as == bs

-- ex 5.3-iii
-- apparently, I need to add the Eq constraint here, but I'm not sure why that isn't inferred automatically.
-- Probably because there is no reason why from "All Eq" would follow "All Ord"
instance (All Eq ts, All Ord ts) => Ord (HList ts) where 
    HNil `compare` HNil = EQ
    (a :# as) `compare` (b :# bs) = case a `compare` b of
                                      EQ -> as `compare` bs
                                      LT -> LT
                                      GT -> GT

instance All Show ts => Show (HList ts) where 
    show HNil = "HNil"
    show (a :# as) = show a ++ " : " ++ show as
