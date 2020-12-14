{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}

-- now try composition for the new lenses
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a


(@.) :: Lens' b c -> Lens' a b -> Lens' a c
-- f :: (c -> f c) -- f acts on c
-- (@.) _c _b = \f -> _b (_c f) -- (_c f) acts on b. And (_b ...) acts on a. Simplify:
-- (@.) _c _b = _b . _c -- simplify more
(@.) = flip (.)
