{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

import GHC.TypeLits (KnownSymbol, symbolVal)
import Data.Proxy

newtype s >> a = Named a

data HRec xs where 
    HEmpty :: HRec '[]
    HCons :: (s >> a) -> HRec xs -> HRec (s >> a ': xs)

instance Show (HRec '[]) where 
    show _ = "HEmpty"

instance (Show a, KnownSymbol s, Show (HRec xs)) => Show (HRec (s >> a ': xs)) where 
    show (HCons (Named a) rest) = 
        let val = show a
            key = symbolVal (Proxy :: Proxy s)
            more = show rest
        in "(" ++ key ++ ": " ++ val ++ ") " ++ more

hr1 = HCons (Named @"foo" 'a') (HCons (Named @"bar" (3 :: Int)) HEmpty)
-- so... we have extensible records where the keys are in the type system.
-- and when I show them, I print the types, so it looks like the types are values. sorta. lol.
-- not sure what is the exact purpose of this design.

-- todo: write toJSON and fromJSON instances
