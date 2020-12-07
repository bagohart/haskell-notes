-- A point in the plane.
data Point = Point
    { positionX :: Double
    , positionY :: Double
    } deriving (Show)

-- A line segment from one point to another.
data Segment = Segment
    { segmentStart :: Point
    , segmentEnd :: Point
    } deriving (Show)

makePoint :: (Double, Double) -> Point
makePoint (x,y) = Point x y

makeSegment :: (Double, Double) -> (Double, Double) -> Segment
makeSegment start end = Segment (makePoint start) (makePoint end)

testSeg = makeSegment (0,1) (2,4)
e1 = positionY . segmentEnd $ testSeg

update1 = testSeg { segmentEnd = makePoint (2,3) }
update2 = let end = segmentEnd testSeg in testSeg { segmentEnd = end { positionY = 2 * positionY end } }
