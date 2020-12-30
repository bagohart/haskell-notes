{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE RankNTypes #-}

module Ex6 where 

import Lens.Micro.Platform
import Test.Hspec
import Data.Monoid (Endo)

-- | map/fmap
mapLens :: ASetter s t a b -> (a -> b) -> s -> t
mapLens = over

-- | toList
toListLens :: Getting (Endo [a]) s a -> s -> [a]
toListLens ln s = s^..ln
-- this is... a bit lame.

-- | catMaybes
catMaybesLens :: [Maybe a] -> [a]
catMaybesLens s = s^..folded._Just
-- solution uses each._Just. hm. what is this even about... ?_?

main6 :: IO ()
main6 = hspec $ do
  it "mapLens" $
    mapLens _2 not ((), True) `shouldBe` ((), False)
  it "toListLens" $
    toListLens both ('x', 'y') `shouldBe` "xy"
  it "catMaybesLens" $
    catMaybesLens [Just 'x', Nothing, Just 'y'] `shouldBe` "xy"
