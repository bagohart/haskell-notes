{-# LANGUAGE Arrows,InstanceSigs #-}

import Control.Arrow
import qualified Control.Category as Cat
import qualified Data.Bifunctor as Bi

count1 w = length . filter (==w) . words

-- rejected
-- count2 w = print . length . filter (==w) . words . readFile

-- this works but is ugly:
count2' :: String -> FilePath -> IO ()
count2' w = (>>=print) .
    fmap (length . filter (==w) . words) .
        readFile

type Kleisli1 m a b = a -> m b

(>>!) :: Monad m => Kleisli1 m a b -> Kleisli1 m b c -> Kleisli1 m a c
(>>!) g h = \a -> g a >>= h

-- now I can pointfree:
printFile = readFile >>! print

arr1 :: Monad m => (a -> b) -> Kleisli1 m a b
arr1 g = pure . g

-- now it's much nicer. the >>! is so beautiful :)
count3 :: String -> FilePath -> IO ()
count3 w = readFile >>! arr1 words >>! arr1 (filter (==w)) >>! arr1 length >>! print

-- we can overload the notation >>> to use Arrows and the predefined Kleisli thingy, then we get:
count4 :: String -> Kleisli IO FilePath ()
count4 w = Kleisli readFile >>> arr words >>> arr (filter (==w)) >>> arr length >>> Kleisli print
-- which is a bit less nice but uniform which is probably also nice I guess or something.

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

-- example usage:
ex1 = runSF (arr (+1)) [1..5]

delay :: a -> SF a a
delay x = SF (x:)

ex2 = runSF (delay 0) [1..5]
