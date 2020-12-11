{-# LANGUAGE Arrows,InstanceSigs #-}

import Control.Arrow
import qualified Control.Category as Cat
import qualified Data.Bifunctor as Bi

-- 2.1
addM :: Monad m => m Int -> m Int -> m Int
addM a b = do
    x <- a
    y <- b
    return (x+y)

-- express the same thing with arrows:
addA :: Arrow arr => arr a Int -> arr a Int -> arr a Int
addA g h = g &&& h >>> arr (uncurry (+))

-- let's implement &&& for stream functions:

-- stream functions
newtype SF a b = SF { runSF :: [a] -> [b] }
-- those aren't actually Arrows according to the Arrow laws. What o_O

instance Cat.Category SF where 
    id :: SF a a
    id = SF id

    (.) :: SF b c -> SF a b -> SF a c
    (.) (SF h) (SF g) = SF (h . g)

instance Arrow SF where 
    arr :: (a -> b) -> SF a b
    arr g = SF (fmap g)

    first :: SF a b -> SF (a,d) (b,d)
    first (SF g) = SF $ uncurry zip . (Bi.first g) . unzip
    -- or using the arrow instance of functions:
    -- first (SF g) = SF (unzip >>> first f >>> uncurry zip)

    (&&&) :: SF a b -> SF a b' -> SF a (b,b')
    (&&&) (SF g) (SF h) = SF $ \as -> zip (g as) (h as)
    -- or pointfree:
    -- (&&&) (SF g) (SF h) = SF $ zip <$> g <*> h
    -- or reusing the arrow operators on functions:
    -- (&&&) (SF g) (SF h) = SF $ g &&& h >>> uncurry zip
    -- which is actually longer than using the applicative on functions as above t_t

    (***) :: SF a b -> SF c d -> SF (a,c) (b,d)
    (***) (SF g) (SF h) = SF $ uncurry zip . Bi.bimap g h . unzip

delay :: a -> SF a a
delay x = SF (x:)

-- pairPred :: Arrow arr => arr Int (Int,Int)
pairPred = arr id &&& delay 0

expp = runSF (arr id &&& delay 0) [1..5]

-- now: ArrowChoice.
mapA :: ArrowChoice arr => arr a b -> arr [a] [b]
mapA f = arr listcase >>> arr (const []) ||| (f *** mapA f >>> arr (uncurry (:)))
-- recursion on arrows always looks a bit mysterious.

listcase :: [a] -> Either () (a,[a])
listcase [] = Left ()
listcase (x:xs) = Right (x,xs)

-- newtype SF a b = SF { runSF :: [a] -> [b] }

instance ArrowChoice SF where 
    left :: SF b c -> SF (Either b d) (Either c d)
    left (SF g) = SF $ \xs -> combine xs (g [y | Left y <- xs])
        where combine (Left y:xs) (z:zs) = Left z : combine xs zs -- note that z is the transformed y!
              combine (Right y:xs) zs = Right y : combine xs zs
              combine [] zs = []
    -- Have: [b] -> [c]. Build: [Either b d] ~> map:
    -- Right d -> Right d
    -- Left b -> [b] -> nope, this doesn't work, I need to look at all the values at once. but how to merge them?
    -- Solution here assumes that one input produces one output. hm.

-- earlier definition of delay is not a synchronous stream function, i.e. the output length != input length
delay2 x = SF (init . (x:))
-- so we cut off the last element and are happy. uh oh.

exsf = runSF (mapA (delay2 0)) [[1,2,3],[4,5,6],[7,8,9]]
-- dafuq

exsf2 = runSF (mapA (delay2 0)) [[1,2,3],[4,5],[6],[7,8],[9,10,11],[12,13,14,15]]
-- what is this black magic

delaysA = arr listcase >>>
    arr (const []) |||
        (arr id *** (delaysA >>> delay2 []) >>>
            arr (uncurry (:)))
-- what does this even do

exsf3 = runSF delaysA [[1,2,3],[4,5,6],[7,8,9],[10,11,12]]
-- it delays columns by different amounts. wtf.

-- feedback

nor :: SF (Bool,Bool) Bool
nor = arr (not . uncurry (||))

edge :: SF Bool Bool
edge = arr id &&& delay2 False >>> arr detect
    where detect (a,b) = a && not b

instance ArrowLoop SF where 
    loop (SF g) = SF $ \as ->
        let (bs,cs) = unzip (g (zip as (stream cs))) in bs
            where stream ~(x:xs) = x : stream xs -- stream bot = bot : bot : bot : ...

-- flipflop =
--     loop (arr (\((reset,set),(c,d)) -> ((set,d),(reset,c))) >>>
--         nor *** nor >>>
--             arr id &&& arr id)
--  ^ this doesn't work. is circular: ith ouput depends on itself. not sure where in that equation this would be visible.

flipflop =
    loop (arr (\((reset,set),~(c,d)) -> ((set,d),(reset,c))) >>>
        nor *** nor >>>
            delay (False,True) >>>
                arr id &&& arr id)
-- I'm not sure this is an improvement over other approaches.
-- This seems not even to work exactly as desired. Hm.
-- Also this article hints at overloading delay, but it's not even clear how this would be done
-- since I have 2 different implementations of delay even in this small example... ?_?

-- higher order arrows
newtype ArrowMonad1 arr a = ArrowMonad1 (arr () a)

instance ArrowApply a => Applicative (ArrowMonad1 a)

instance ArrowApply a => Functor (ArrowMonad1 a)

instance ArrowApply a => Monad (ArrowMonad1 a) where 
    return x = ArrowMonad1 (arr (const x))
    (>>=) :: ArrowMonad1 a b -> (b -> ArrowMonad1 a c) -> ArrowMonad1 a c
    (>>=) (ArrowMonad1 m) g = ArrowMonad1 $
        m >>> arr (\x -> let ArrowMonad1 h = g x in (h, ())) >>> app

-- exercise: filtering
filterA :: ArrowChoice arr => arr a Bool -> arr [a] [a]
filterA f = mapA f &&& Cat.id >>> arr (concat . uncurry (zipWith op))
    where op :: Bool -> a -> [a]
          op True x = [x]
          op False _ = []

exp1 = runSF (filterA (arr even >>> delay True)) [[1,2,3],[4,5,6],[7,8,9]]
exp2 = runSF (filterA (arr even >>> delay True)) [[1,2,3],[4,5],[6],[7,8],[9,10,11],[12,13,14,15]]
-- ok, this is magic :)
-- I would probably have to read something on stream functions to understand
-- all the implicit ideas that are contained here.
