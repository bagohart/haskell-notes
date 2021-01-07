{-# LANGUAGE ScopedTypeVariables, TypeApplications, AllowAmbiguousTypes, TypeFamilies #-}

import Data.Typeable

-- 4.1
broken :: forall a b. (a -> b) -> a -> b
broken f a = apply
    where 
        apply :: b -- need both the ScopedTypeVariables extension and the explicit forall for this to work!
        apply = f a

-- some older libraries use a Proxy thing to keep type information:
-- data Proxy a = Proxy
-- as in
-- typeRep :: Typeable a -> Proxy a -> TypeRep

-- 4.2
-- :t fmap @Maybe yields fmap :: (a -> b) -> Maybe a -> Maybe b
-- usage: applies the first thing first. skip with _
-- this works a bit differently than the book states though...
-- :t fmap @_ @Int @Bool :: ...
-- => be careful about type order when writing a function, probably use forall to make it explicit and change it.
-- type applications can be used for functions, but also for data constructors, see below...

-- 4.3
typeName :: forall a. Typeable a => String -- this needs AllowAmbiguousTypes to compile
-- because a doesn't appear right of "=>"
typeName = show . typeRep $ Proxy @a

-- use this function with typeName @String:
lol = typeName @String
haha = typeName @(Maybe (String -> Int))

-- Proxy @a is a shorthand for Proxy :: Proxy a
-- this is a very simple type application.

type family AlwaysUnit a where 
    AlwaysUnit a = ()

-- 1. AlwaysUnit a -> a
-- ^ a appears on the right side, so this shouldn't be ambiguous... right?
-- 2. b -> AlwaysUnit a -> b
-- ^ appears on the left side of the right side, so... hm. ?_?
-- 3. Show a => AlwaysUnit a -> String
-- ^ I know _something_ about a, but is that enough to make it non-ambiguous? hm.
-- According to the book this is in fact ambiguous because I can't get a from AlwaysUnit a,
-- at least not using a type family called Inverse. This is not injective... ok.
-- I guess this seems confusing since I don't really get the type family.
-- It's like a constant function that maps every type to (), ok.
-- But then what does it even mean to write a signature such as "Show a => AlwaysUnit a -> String" ?
-- This should be basically "() -> String"
-- So I understand neither the type family thing nor the ambiguity thing clearly right now.
-- Maybe it becomes clearer later ...

