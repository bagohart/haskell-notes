{-# LANGUAGE Arrows,InstanceSigs #-}

import Control.Arrow
import Control.Monad
import qualified Control.Category as Cat
import Data.List
import Data.Maybe
import System.Random

-- if f (a -> b) really has an arrow instance I should be able to implement it, right?
newtype StaticMorphism f a b = StaticMorphism (f (a -> b))

instance Applicative f => Cat.Category (StaticMorphism f) where 
    id :: StaticMorphism f a a
    id = StaticMorphism $ pure id

    (.) :: StaticMorphism f b c -> StaticMorphism f a b -> StaticMorphism f a c
    (.) (StaticMorphism fbc) (StaticMorphism fab) = StaticMorphism $ (.) <$> fbc <*> fab

instance Applicative f => Arrow (StaticMorphism f) where 
    arr :: (a -> b) -> StaticMorphism f a b
    arr f = StaticMorphism $ pure f

    first :: StaticMorphism f a b -> StaticMorphism f (a,c) (b,c)
    first (StaticMorphism fab) = StaticMorphism $ op <$> fab
        where op :: (a -> b) -> (a,c) -> (b,c)
              op g = \(a,c) -> (g a,c)
    -- or just:
    -- first (StaticMorphism fab) = StaticMorphism $ first <$> fab -- functions have an arrow instance
    -- so I can reuse that one

-- exercise 3
liftY2 :: Arrow y => (a -> b -> c) -> y r a -> y r b -> y r c
liftY2 g yra yrb = (yra &&& yrb) >>> arr (uncurry g)
