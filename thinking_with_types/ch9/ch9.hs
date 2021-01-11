{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Kind (Type)
import Data.Monoid ((<>))
import Data.Proxy (Proxy (..))
import GHC.TypeLits

-- printf's format string "%c%d%d" is like "Char -> Int -> Int -> String"
-- "some number: %d" is like \s -> "some number: " <> show s

data (a :: k1) :<< (b :: k2)
infixr 5 :<<
-- hey, :<< looks like << from C++ >_>
-- :<< doesn't have data constructors. this is on purpose, we only want to build this on the type level.

-- :set -XDataKinds
-- :set -XTypeOperators

-- given Int :<< ":" :<< Bool :<< "!", produce Int -> Bool -> String

-- this is an associated type family, and I don't really know what it is...
class HasPrintf a where 
    type Printf a :: Type
    format :: String -> Proxy a -> Printf a

-- instance HasPrintf (text :: Symbol) where 
--     type Printf text = String

-- instance HasPrintf a => HasPrintf ((text :: Symbol) :<< a) where 
--     type Printf (text :<< a) = Printf a

-- instance HasPrintf a => HasPrintf ((param :: Type) :<< a) where 
--     type Printf (param :<< a) = param -> Printf a

instance KnownSymbol text => HasPrintf (text :: Symbol) where 
    type Printf text = String
    format s _ = s <> symbolVal (Proxy @text)

instance (HasPrintf a, KnownSymbol text) => HasPrintf ((text :: Symbol) :<< a) where 
    type Printf (text :<< a) = Printf a
    format s _ = format (s <> symbolVal (Proxy @text)) (Proxy @a)

instance (HasPrintf a, Show param) => HasPrintf ((param :: Type) :<< a) where 
    type Printf (param :<< a) = param -> Printf a
    format s _ param = format (s <> show param) (Proxy @a)

printf :: HasPrintf a => Proxy a -> Printf a
printf = format ""

-- this removes the ugly double quotes when using Strings
instance {-# OVERLAPPING #-} HasPrintf a => HasPrintf (String :<< a) where 
    type Printf (String :<< a) = String -> Printf a
    format s _ param = format (s <> param) (Proxy @a)

test1 = printf (Proxy @"test")
test2 = printf (Proxy @(Int :<< "+" :<< Int :<< "=3")) 1 2
test3 = printf (Proxy @(String :<< " world!")) "hello"
-- this works. what is this black magic o_O

