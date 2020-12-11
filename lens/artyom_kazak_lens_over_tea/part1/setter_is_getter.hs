{-# LANGUAGE InstanceSigs #-}

-- this is a functor. also it stores a value of type x. mm hm.
data Storey x f a = Storey x (f a) deriving Show
-- so basically, this is isomorphic to (x, f a)

instance Functor f => Functor (Storey x f) where 
    fmap :: (a -> b) -> Storey x f a -> Storey x f b
    fmap g (Storey x fa) = Storey x (g <$> fa)

-- this allows us to remove the "original-value-add-on" from our previous lens type:
-- f should be a Functor
type Lens f s a = (a -> f a) -> s -> f s
-- recall, before:
-- type Lens m s a = (a -> m a) -> s -> (a, m s)

-- ix has become a bit simpler:
ix :: Functor f => Int -> Lens f [a] a
ix index f list
    | index < 0 = error "ix: negative index :("
    | null list = error "ix: index too large :("
    | old:rest <- list = if index == 0
                            then (: rest) <$> (f old) 
                            else (old :) <$> ix (index-1) f rest

-- this generates 4 new lists, und also saves the 4 in Storey
theTrick1 = ix 2 (\x -> Storey x [1..x]) [300,100,4,600,900,400]

