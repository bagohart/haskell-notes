import Control.Monad
import Data.Foldable
import Control.Monad.ST
import Data.STRef

sumST :: Num a => [a] -> a
sumST xs = runST $ do
    n <- newSTRef 0 -- n is a STRef s Int
    for_ xs $ \x ->
        modifySTRef n (+x) -- ST s ()
    readSTRef n
