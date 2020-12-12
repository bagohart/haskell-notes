{-# LANGUAGE InstanceSigs #-}

import Data.Functor.Compose
import Data.Functor.Identity

type Lens f s t a b = (a -> f b) -> s -> f t

-- ix has become a bit simpler:
ix :: Functor f => Int -> Lens f [a] [a] a a
ix index f list
    | index < 0 = error "ix: negative index :("
    | null list = error "ix: index too large :("
    | old:rest <- list = if index == 0
                            then (: rest) <$> (f old) 
                            else (old :) <$> ix (index-1) f rest

-- recall:
-- theTrick3 = runIdentity $ ix 2 (\x -> Identity 88) [0..4]
-- and now let's extract the runIdentity/Identity thing into its own function:
over :: Lens Identity s t a b -> ((a -> b) -> s -> t)
over l g = runIdentity . l (Identity . g)
-- the type thingy is still broken, but we'll... get to that?_?

-- now rewrite those:
-- theTrick3 = runIdentity $ ix 2 (\x -> Identity 88) [0..4]
-- theTrick3' = ix 2 (\x -> Identity (x * 4)) [0..4]
theTrick3 = over (ix 2) (const 88) [0..4]
theTrick3' = over (ix 2) (*4) [0..4]
-- it's starting to look a bit nicer already.

