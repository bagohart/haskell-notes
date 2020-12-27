{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, RankNTypes, TypeFamilies #-}

import Data.Int
import Data.Bifunctor
import Data.Functor.Const

traverse1 :: Applicative f => (a -> f b) -> [a] -> f [b]
traverse1 g [] = pure []
traverse1 g (x:xs) = (:) <$> g x <*> traverse1 g xs

traverseIndexed1 :: Applicative f => (Int -> a -> f b) -> [a] -> f [b]
traverseIndexed1 g xs = go g xs 0
    where
        go _ [] _ = pure []
        go g (x:xs) i = (:) <$> (g i x) <*> go g xs (i+1)
-- the solution is almost identical only the arguments to go are in a different order

-- let's build something that can be ordinary or indexed traversal
class Indexable i p where -- i is type of index. could be Int, Int64, some String, something else...
    indexed :: p a b -> (i -> a -> b)

-- a function can just ignore its index and be an indexable function this way :)
instance Indexable i (->) where 
    indexed = const

-- instance for Indexable i (i -> a -> b) ; how to write p?
newtype Indexed i a b = Indexed { runIndexed :: i -> a -> b }

instance (i ~ j) => Indexable i (Indexed i) where 
    indexed = runIndexed

type IndexedTraversal i s t a b = forall p f. (Indexable i p, Applicative f) => p a (f b) -> s -> f t

itraversed :: IndexedTraversal Int [a] [b] a b
itraversed f = go (0 :: Int)
    where 
        go _ [] = pure []
        go i (x:xs) = (:) <$> (indexed f) i x <*> go (i+1) xs

-- instance Indexable Int (Indexed Int64) where 
--     indexed p = \i a -> (runIndexed p) (fromIntegral i) a

-- according to the text, this should now compile, but it doesn't. not sure what's going on here.
-- test = itraversed (Indexed (\i c -> print (i, c))) "abc"
test = itraversed (Indexed (\i c -> print (i::Int, c))) "abc"

-- let's avoid the newtyping thingy here.
itraverseOf :: (Indexed i a (f b) -> s -> f t) -> (i -> a -> f b) -> s -> f t
itraverseOf l f = l (Indexed f)

test2 = itraverseOf itraversed (\i c -> print (i::Int,c)) "abc"

-- idea for creating indexed traversals automatically:
-- add State with an index-counter, and somehow mix it with Applicative applications...
newtype Indexing f a = Indexing { runIndexing :: Int -> (Int, f a) }

-- this is missing from the tutorial, but seems pretty straigtforward
-- Actually, the weaker Functor f condition should be enough to here!
instance Applicative f => Functor (Indexing f) where 
    fmap g (Indexing h) = Indexing $ \i -> second (fmap g) (h i)

-- run the first function first, then run the second function with the new index, then 
-- combine the results and give back the latest index or something.
instance Applicative f => Applicative (Indexing f) where 
    pure x = Indexing $ \i -> (i, pure x)

    Indexing mf <*> Indexing ma = Indexing $ \i ->
        case mf i of
          (j, ff) -> case ma j of
            (k, fa) -> (k, ff <*> fa)

-- the first argument is an ordinary traversal. Since an ordinary traversal works on any Functor, we can just choose
-- (Indexing f) as the Functor!
-- The return type is an Int-indexed traversal.
indexing :: ((a -> Indexing f b) -> s -> Indexing f t) -> Indexed Int a (f b) -> s -> f t
-- stuff below gets us an Indexing, so let's throw 0 on that and throw away the final index
indexing l = \f s ->
    -- l :: (a -> Indexing f b) -> s -> Indexing f t
    -- f :: Indexed Int a (f b)
    -- s :: s
    let
        -- l expects an Indexing, but we only have an Indexed, so turn the Indexed into Indexing:
        f' a = Indexing $ \i -> (i+1, (runIndexed f) i a)
     in snd $ runIndexing (l f' s) 0

-- newtype Indexing f a = Indexing { runIndexing :: Int -> (Int, f a) }
-- newtype Indexed i a b = Indexed { runIndexed :: i -> a -> b }

test3 = itraverseOf (indexing traverse) (\i a -> print (i, a)) "abc"

indexing2 :: Indexable Int p => ((a -> Indexing f b) -> s -> Indexing f t) -> p a (f b) -> s -> f t
indexing2 l = \f s ->
    let
        -- l expects an Indexing, but we only have an Indexed, so turn the Indexed into Indexing:
        f' a = Indexing $ \i -> (i+1, (indexed f) i a)
     in snd $ runIndexing (l f' s) 0

test4 = itraverseOf (indexing2 traverse) (\i a -> print (i::Int, a)) "abc"

-- <. .> <.>
-- exercise: ^@.. aka toListOfIndexed or something
type IndexedGetting i m s a = Indexed i a (Const m a) -> s -> Const m s
type Getting r s a = (a -> Const r a) -> s -> Const r s

toListOfIndexed :: IndexedGetting i [(i,a)] s a -> s -> [(i,a)]
toListOfIndexed l = \s -> getConst $ l (Indexed (\i x -> Const [(i,x)])) s
    -- idea:
    -- toListOf puts the things into singleton lists wrapped in Const.
    -- now just do the same, but wrap the elements in tuples with the index as its first element.
    -- (this is the 'subtly broken' version of toListOf with horrible performance due to list concatenation,
    -- but I don't care for now.

-- implementation of traversed is somewhere else =/
test5 = toListOfIndexed (indexing traverse1)  "abc"
test6 = toListOfIndexed (indexing (traverse1.traverse1))  ["abcd", "efgh"]
-- ^ this compiles, but gives different results ... ?_?
-- not sure where I mixed up...
-- toListOfIndexed cannot actually modify the traversal in anyway, so traversed probably does something
-- I don't currently expect?
test8 = toListOfIndexed (indexing traverse1)  ["abcd", "efgh"]

-- combine both indices into a tuple
(<.>) :: (Indexed i     a (f b) -> s -> f t)
      -> (Indexed   j   x (f y) -> a -> f b)
      -> (Indexed (i,j) x (f y) -> s -> f t)
(<.>) abst xyab (Indexed fij) s = abst (Indexed ab) s
    -- abst :: Indexed i a (f b) -> s -> f t
    -- xyab :: Indexed j x (f y) -> a -> f b
    -- fij :: (i,j) -> x -> f y
    -- s :: s
    where 
        -- ab :: i -> a -> f b
        ab i a = xyab (Indexed xy) a
            where 
                xy j x = fij (i,j) x
                -- xy :: j -> x -> f y

-- newtype Indexed i a b = Indexed { runIndexed :: i -> a -> b }

(<.>>) :: Indexable (i,j) p
      => (Indexed i     a (f b) -> s -> f t)
      -> (Indexed   j   x (f y) -> a -> f b)
      -> (p x (f y) -> s -> f t)
(<.>>) abst xyab fij s = abst (Indexed ab) s
    -- abst :: Indexed i a (f b) -> s -> f t
    -- xyab :: Indexed j x (f y) -> a -> f b
    -- fij :: (i,j) -> x -> f y
    -- s :: s
    where 
        -- ab :: i -> a -> f b
        ab i a = xyab (Indexed xy) a
            where 
                xy j x = (indexed fij) (i,j) x
                -- xy :: j -> x -> f y

(<.) :: Indexable i p
      => (Indexed i     a (f b) -> s -> f t)
      -> (Indexed   j   x (f y) -> a -> f b)
      -> (p x (f y) -> s -> f t)
(<.) abst xyab fij s = abst (Indexed ab) s
    where 
        ab i a = xyab (Indexed xy) a
            where 
                xy j x = (indexed fij) i x

(.>) :: Indexable j p
      => (Indexed i     a (f b) -> s -> f t)
      -> (Indexed   j   x (f y) -> a -> f b)
      -> (p x (f y) -> s -> f t)
(.>) abst xyab fij s = abst (Indexed ab) s
    where 
        ab i a = xyab (Indexed xy) a
            where 
                xy j x = (indexed fij) j x

-- the types in the tutorial are a bit different, but I don't see exactly which advantage that gives us right now.
