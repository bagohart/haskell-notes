{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- -- type level numbers
-- data Zero
-- data Succ a
-- type One = Succ Zero
-- type Two = Succ One
-- type Three = Succ Two

-- this is allowed but it probably shouldn't be...
-- type Nonsense = Succ Bool

-- now in kind safe
data Nat = Zero | Succ Nat

-- now we have 'Zero and 'Succ. They are types. But they don't have values, because they're not *

-- let's do GADTs
data IntBool a where 
    Int :: Int -> IntBool Int
    Bool :: Bool -> IntBool Bool

extractIntBool :: IntBool a -> a
extractIntBool (Int _) = 0
extractIntBool (Bool b) = b

testExtract1 = extractIntBool (Int 5)
testExtract2 = extractIntBool (Bool True)

-- let's do vector
--           length   content
data Vector (n :: Nat) (a :: *) where 
    VNil :: Vector 'Zero a
    VCons :: a -> Vector n a -> Vector ('Succ n) a

-- this is no problem: the n doesn't influence this at all
instance Show a => Show (Vector n a) where 
    show VNil = "VNil"
    show (VCons a as) = "VCons " ++ show a ++ " (" ++ show as ++ ")"

append :: Vector n a -> Vector m a -> Vector (Add n m) a
-- append VNil rest = VNil -- this won't compile, and for good reason!
append VNil xs = xs -- go check :t append VNil with undefined definition, then let the type guide you
-- append (VCons a rest) xs = append rest (VCons a xs) -- this compiles although it is broken
append (VCons a rest) xs = VCons a (append rest xs) -- this is correct, but ghc can't figure this out.
-- because is Add n (Succ m) = Succ (Add n m) ? Actually yes, sorta, but structurally no.
-- we can fix this by changing the type family (1)

type family Add n m where 
    Add 'Zero n = n
    -- Add ('Succ n) m = Add n ('Succ m) -- see (1) why this attempt didn't work
    Add ('Succ n) m = 'Succ (Add n m)
-- according to the tutorial, I should be able to evaluate this with ghci but I get

