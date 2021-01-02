{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, PolyKinds #-}

import GHC.TypeLits

data UserType = NoAdmin | Admin

-- this doesn't compile, probably because of this line
-- how do I declare that a has kind UserType?
data Proxy a = Proxy

-- data User = User { userAdminToken :: Maybe (Proxy 'Admin) }

-- doSensitiveThings :: Proxy 'Admin -> IO ()
-- doSensitiveThings = undefined

type family Or (x :: Bool) (y :: Bool) :: Bool where 
    Or 'True  y = 'True
    Or 'False y = y

-- exercise 2.4-i
type family Not (x :: Bool) :: Bool where 
    Not 'True = 'False
    Not 'False = 'True

type family Map (x :: a -> b) (i :: [a]) :: [b] where 
    Map f '[] = '[]
    Map f (x ': xs) = f x ': Map f xs

type family Foo (x :: Bool) (y :: Bool) :: Bool
type family Bar x y :: Bool -> Bool -> Bool
