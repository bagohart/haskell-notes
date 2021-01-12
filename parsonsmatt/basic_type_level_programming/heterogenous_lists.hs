{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

data HList xs where 
    HNil :: HList '[]
    (:::) :: a -> HList as -> HList (a ': as)

infixr 6 :::

-- instance Show (HList xs) where 
--     show HNil = "HNil"
--     show (x ::: rest) = "_ ::: " ++ show rest

instance Show (HList '[]) where 
    show HNil = "HNil"

instance (Show (HList as), Show a) => Show (HList (a ': as)) where 
    show (a ::: rest) = show a ++ " ::: " ++ show rest

-- todo: aeson instance
