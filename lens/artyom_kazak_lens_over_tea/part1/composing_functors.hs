import Data.Functor.Compose

type Storey x f = Compose ((,) x) f

theTrick1 = ix 2 (\x -> Storey x [1..x]) [300,100,4,600,900,400]
