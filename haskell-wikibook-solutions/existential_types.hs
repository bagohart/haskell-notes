{-# LANGUAGE ExistentialQuantification, RankNTypes, InstanceSigs #-}

map' :: forall a b. (a -> b) -> [a] -> [b]
map' = map

id' :: forall a. a -> a
id' = id

data ShowBox = forall s. Show s => SB s 

instance Show ShowBox where 
    show :: ShowBox -> String
    show (SB s) = "Box: <<" ++ show s ++ ">>"

heteroList :: [ShowBox]
heteroList = [SB (), SB 5, SB True]

f :: [ShowBox] -> IO ()
f xs = mapM_ print xs

-- what does this mean?
-- forall a. a ~~ intersection of all types = {bottom}
-- this means all types, because bottom is everywhere.
-- What can you do with this? everything you can do with bottom.
--
-- [forall a. a] ~~ list of bottoms
-- [forall a. Show a => a] list of elements with type forall a. Show a => a
-- ^ this is... also a list of bottoms.
-- [forall a. Num a => a] ~~ list of elements with type forall a. Num a => a
-- ^ is this... also bottoms because you can create pseudo numbers? maybe?
-- forall a. [a] ~~ type of a list where all elements have the same type.
-- ^ oh and it is a list of bottoms.
-- ... ??? it seems we always get bottoms so how is this comparison helpful?

data T = forall a. MkT a

foo (MkT x) = undefined -- x could be anything, e.g. it's sorta a member of {bottom} o_O

hList1 :: [T]
hList1 = [MkT 5, MkT (), MkT True, MkT map]
-- this works. We can't DO anything with these values, but hey there's always something...

data T' = forall a. Show a => MkT' a

hList2 :: [T']
hList2 = [MkT' 5, MkT' (), MkT' True, MkT' "sartre"]

pr = mapM_ (\(MkT' x) -> print x) hList2

-- products and sums
-- id :: forall a. a -> a
-- id a = a
-- ^ ~~ a product/typle of functions, one per type a.
-- => this is an infinite product of functions:
-- id :: Int Int x id String String x id [Int] [Int] x ...
-- ^ to construct such a value, we have to provive all components of the tuple, i.e. infinitely many.
-- luckily, the id a = a thing does this.
--
x :: forall a. Num a => a
x = 0
-- x ~~ Int x Double x Float x ...

-- data ShowBox = forall s. Show s => SB s 
-- data ShowBox = SBUnit | SBInt | SBBool | SBIntList | ...
-- To construct such a value, pick one of the constructors.
-- Not sure how this + infinity is different from the x infinity.
-- apparently, this existential type thingy does something very different than the universality type thingy...?_?
-- I think this is skipping something. Probably a lot.
-- Also the existential type has only a forall quantifier which seems... surprising?
--

-- Let's build pairs
-- this is what we can do without fancy features
data Pair a b = P a b deriving (Eq,Show)

makePair :: a -> b -> Pair a b
makePair x y = P x y

runPair :: Pair a b -> (a -> b -> c) -> c
runPair (P x y) = \f -> f x y

p = makePair "a" 'b'

rp1 = runPair p $ \x y -> x
rp2 = runPair p $ \x y -> y

-- now use fancy features

newtype Pair' a b = Pair' (forall c. (a -> b -> c) -> c)
-- a pair is just a constructor around a function that takes a function to produce pairs and produces pairs... ???
runPair' (Pair' f) = f

makePair' :: a -> b -> Pair' a b
makePair' a b = Pair' $ \f -> f a b

p' = makePair' "a" 'b'

rp1' = runPair' p' $ \x y -> x
rp2' = runPair' p' $ \x y -> y

-- I fail to see how I now invented something more powerful than without the fancy features.
-- No new syntax, either.
-- What is this about ???
