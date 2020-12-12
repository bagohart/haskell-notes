{-# LANGUAGE RankNTypes, TupleSections #-}

import Data.Functor.Identity

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a

_abs :: Real a => Lens' a a
_abs f n = update <$> f (abs n)
    where
        update x
            | x < 0 = error "_abs: absolute value can't be negative"
            | otherwise = signum n * x

dafuq = over _abs (^2) (-10)
-- I can lens into a real number and... change its absolute value o_O

over :: Lens s t a b -> ((a -> b) -> s -> t)
over l g = runIdentity . l (Identity . g)

set :: Lens s t a b -> b -> s -> t
set l b = over l (const b)

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set = \h s -> set s <$> h (get s)

_all :: Eq a => a -> Lens' [a] a
_all ref = lens get set
    where 
        get = const ref -- I get the a even if it is not contained in the list :)
        set s new = map (\old -> if old == ref then new else old) s -- I set only the same elements

dafuq2 = set (_all 0) (-8) [100,600,0,200,0]

dafuq3 = (_all 0) (const $ putStr "? new: " >> readLn) [100,600,0,200,0]
