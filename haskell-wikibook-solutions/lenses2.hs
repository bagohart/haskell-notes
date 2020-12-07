{-# LANGUAGE TemplateHaskell, RankNTypes #-}

import Data.Functor.Identity

-- run this with $ stack ghci --package lens
import Control.Lens
import Control.Lens.Getter

data Point = Point
    { _positionX :: Double
    , _positionY :: Double
    } deriving (Show)
makeLenses ''Point

data Segment = Segment
    { _segmentStart :: Point
    , _segmentEnd :: Point
    } deriving (Show)
makeLenses ''Segment

makePoint :: (Double, Double) -> Point
makePoint (x,y) = Point x y

makeSegment :: (Double, Double) -> (Double, Double) -> Segment
makeSegment start end = Segment (makePoint start) (makePoint end)

testSeg :: Segment
testSeg = makeSegment (0,1) (2,4)

v1 :: Point
v1 = view segmentEnd testSeg

s1 :: Segment
s1 = set segmentEnd (makePoint (2,3)) testSeg

v2 :: Double
v2 = view (segmentEnd . positionY) testSeg

o1 :: Segment
o1 = over (segmentEnd . positionY) (2*) testSeg

pointCoordinates :: Applicative f => (Double -> f Double) -> Point -> f Point
pointCoordinates g (Point x y) = Point <$> g x <*> g y

pl :: [Point]
pl = pointCoordinates (\x -> [x,2*x]) (makePoint (1,2))

deleteIfNegative :: (Num a, Ord a) => a -> Maybe a
deleteIfNegative x = if x < 0 then Nothing else Just x

pc1 = pointCoordinates deleteIfNegative (makePoint (1,2))
pc2 = pointCoordinates deleteIfNegative (makePoint (1,-2))

-- data Point = Point { _positionX :: Double
                --     , _positionY :: Double
                --     } deriving (Show)
-- data Segment = Segment { _segmentStart :: Point
                    --     , _segmentEnd :: Point
                    --     } deriving (Show)

-- first try without pointCoordinates, this seems incomplete:
-- extremityCoordinates0 :: Applicative f => (Double -> f b) -> Segment -> f t
-- extremityCoordinates0 g (Segment (Point x1 y1) (Point x2 y2)) = pure ??? <*> g x1
--                                                                         <*> g y1
--                                                                         <*> g x2
--                                                                         <*> g y2

-- Let's fix this attempt.
-- the signature of this function is unclear to me. how do I get the t?
-- Do I need an additional function h, like this: ?
extremityCoordinates1 :: Applicative f => (Double -> f b) -> (b -> b -> b -> b -> t) -> Segment -> f t
extremityCoordinates1 g h (Segment (Point x1 y1) (Point x2 y2)) = pure h <*> g x1
                                                                         <*> g y1
                                                                         <*> g x2
                                                                         <*> g y2

-- how to use pointCoordinates in this?
extremityCoordinates2 :: Applicative f =>
    (Double -> f Double) -> Segment -> f Segment
extremityCoordinates2 g (Segment start end) = Segment <$> pointCoordinates g start
                                                      <*> pointCoordinates g end
-- This seems ok, but the type is not as general as in the Traversal type. what is this exercise about?
over_test1 = over pointCoordinates negate (makePoint (1,2))
over_test2 = over mapped negate [1..4]
over_test3 = over mapped negate $ Just 3

set_test1 = set pointCoordinates 7 (makePoint (1,2))

-- exercises
scaleSegment :: Double -> Segment -> Segment
scaleSegment n s = over extremityCoordinates2 (*n) s

scale_test1 = scaleSegment 10 testSeg

-- mapped' :: (Settable f1, Functor f2) => (a -> f1 b) -> f2 a -> f1 (f2 b)
mapped' :: (Functor f) => (a -> Identity b) -> f a -> Identity (f b)
mapped' g fa = Identity $ fmap (runIdentity . g) fa

mapped_test = over mapped' negate [1..4]

fold_test1 = toListOf extremityCoordinates2 (makeSegment (0,1) (2,3))
fold_test2 = preview extremityCoordinates2 (makeSegment (0,1) (2,3))

-- someGetter :: (a -> Const r a) -> s -> Const r s
-- someGetter g x = Const (someGetter' (getConst . g) x)
--
-- someGetter' :: (a -> r) -> s -> r
-- someGetter' k x = getConst (some

getter_test1 = view (to fst) (4,1)
getter_test2 = view (to _positionX) (makePoint (1,2))

-- exercise: write lenses for the fields of Point and Segment
-- It is not entirely clear to me what the type signature should be, but I can look at what the library
-- generated for me.
_x :: (Functor f) => (Double -> f Double) -> Point -> f Point
_x g (Point x y) = fmap (\x' -> Point x' y) (g x)
-- _x g (Point x y) = (flip Point y) <$> (g x) <- this should work too. It's not obviously better :)
-- wtf is this magic

_y :: Functor f => (Double -> f Double) -> Point -> f Point
_y g (Point x y) = (Point x) <$> (g y)

_start :: Functor f => (Point -> f Point) -> Segment -> f Segment
_start g (Segment start end) = fmap (\start' -> Segment start' end) (g start)

_end :: Functor f => (Point -> f Point) -> Segment -> f Segment
_end g (Segment start end) = fmap (\end' -> Segment start end') (g end)

-- exercise: implement the lens function
lens' :: Functor f => (s -> a) -> (s -> b -> t) -> ((a -> f b) -> s -> f t)
lens' get set = \g x -> set x <$> (g (get x)) 

-- exercise: use lens' to simplify implementation of the previous things
-- aka implement lenses easier
_x' :: (Functor f) => (Double -> f Double) -> Point -> f Point
_x' = lens' _positionX (\point x -> point { _positionX = x })

_y' :: (Functor f) => (Double -> f Double) -> Point -> f Point
_y' = lens' _positionY (\point y -> point { _positionY = y })

_start' :: Functor f => (Point -> f Point) -> Segment -> f Segment
_start' = lens' _segmentStart (\segment start -> segment { _segmentStart = start })

_end' :: Functor f => (Point -> f Point) -> Segment -> f Segment
_end' = lens' _segmentEnd (\segment end -> segment { _segmentEnd = end })

-- data Point = Point
--     { _positionX :: Double
--     , _positionY :: Double
--     } deriving (Show)
-- makeLenses ''Point

-- data Segment = Segment
--     { _segmentStart :: Point
--     , _segmentEnd :: Point
--     } deriving (Show)
-- makeLenses ''Segment

-- todo: prism magic
