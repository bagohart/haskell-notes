{-# LANGUAGE GADTs, EmptyDataDecls, KindSignatures #-}

data Empty
data NonEmpty

data SafeList a b where 
        Nil :: SafeList a Empty
        Cons :: a -> SafeList a b -> SafeList a NonEmpty

safeHead :: SafeList a NonEmpty -> a
safeHead (Cons x _) = x

thisWorks = safeHead (Cons "hi" Nil)
-- thisDoesntWork = safeHead Nil

-- exercise
-- safeTail :: SafeList a NonEmpty -> SafeList a ???
-- safeTail (Cons _ Nil) = Nil
-- safeTail (Cons _ (Cons x xs)) = Cons x xs
-- ^ this approach runs into the same problem as silly, it has to give back both types,
-- and depending on the input value (not type), another type comes back.

-- silly :: Bool -> SafeList a Empty
-- silly :: Bool -> SafeList a NonEmpty
-- silly :: Bool -> SafeList a ??????
-- silly False = Nil
-- silly True = Cons () Nil
-- I can't compile this :'(
-- how can we rescue this?

data NotSafe
data Safe

-- throw away the knowledge that Cons cannot produce an empty list.
-- this seems... not too useful ?_?
data MarkedList :: * -> * -> * where 
    Nil' :: MarkedList t NotSafe
    Cons' :: a -> MarkedList a b -> MarkedList a c

safeHead' :: MarkedList a Safe -> a
safeHead' (Cons' x _) = x

silly :: Bool -> MarkedList () NotSafe
silly False = Nil'
silly True = Cons' () Nil'

-- the resulting list *could* be empty, and this is now reflected in the type system.
-- not bad, actually
safeTail' :: MarkedList a Safe -> MarkedList a NotSafe
safeTail' (Cons' _ Nil') = Nil'
safeTail' (Cons' _ (Cons' x xs)) = Cons' x xs

