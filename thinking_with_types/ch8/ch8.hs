{-# LANGUAGE RoleAnnotations, ScopedTypeVariables, TypeApplications, TypeFamilies #-}

import Data.Coerce (Coercible(..), coerce)
import Data.Foldable (toList)
import qualified Data.Map as M
import Data.Monoid (Sum (..), Product (..))

slowSum :: [Int] -> Int
slowSum = getSum . mconcat . fmap Sum

fastSum :: [Int] -> Int
fastSum = getSum . mconcat . coerce

-- 8.2
-- there are 3 role thingys:
-- nominal: type equality
-- representational: it's safe to reinterpret memory via coerce
-- phantom: ... if it's a phantom type ?_?
--
-- Sum a <- a is "at role representational", and this has something to do with the step from
-- Coercible a b => Coercible (Sum a) (Sum b)
--
-- data Proxy a = Proxy
-- ^ a is at role phantom
-- => Coercible (Proxy a) (Proxy b) is always true.
--
-- ordering #situations with possible coercions: phantom > representational > nominal
-- automatic inference:
-- • everything is phantom
-- • any type parameter applied to (->) becomes representational, also Data constructors
-- • ~ becomes nominal. Also GADTs and type families. what?

-- ex. 8.2-i
-- role signature of Either a b:
-- what is a role signature even ???
-- things are always phantom, so a and b should both be phantom.
-- Either :k: * -> * -> *
-- ^ this is a type constructor, so a and b should be representational
-- What about nominal? Nothing, I guess...
-- this is, according to the solution, written as
-- "type Role Either representational representational"

-- ex. 8.2-ii
-- role signature of Proxy a:
-- this is a phantom thingy, but does this mean that a is only phantom? this would mean that all the types could be
-- used in Proxy, which would be useful, so that's probably it:
-- "type Role Proxy phantom"
-- but Proxy :k: * -> *
-- so again, it should also be representational.
-- but according to the solution, it's not. hm.

type family IntToBool a where 
    IntToBool Int = Bool
    IntToBool a = a

-- ^ if a is at role representational, then
-- Coercible a b => Coercible (IntToBool a) (IntToBool b)
-- which is wrong, so it must be nominal.

data BST v = Empty | Branch (BST v) v (BST v)

type role BST nominal
-- now it is less permissive than what is inferred by the compiler.
-- reasoning: order. as with Map k v.
-- this means we strengthened the role.
--
-- we can not make it phantom, i.e. weaken it.
