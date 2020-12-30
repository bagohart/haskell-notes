{-# LANGUAGE OverloadedStrings #-}

module Ex3 where

import Lens.Micro.Platform
import Data.Text (Text)
import Test.Hspec

data Address = Address
  { _street :: !Text
  , _city :: !Text
  }

street :: Lens' Address Text
street g (Address street city) = fmap (\newStreet -> Address newStreet city) (g street)

city :: Lens' Address Text
city g (Address street city) = fmap (Address street) (g city)

data Person = Person
  { _name :: !Text
  , _address :: !Address
  , _age :: !Int
  }

name :: Lens' Person Text
name g (Person name address age) = fmap (\newName -> Person newName address age) (g name)

address :: Lens' Person Address
address g (Person name address age) = fmap (\newAddress -> Person name newAddress age) (g address)

age :: Lens' Person Int
age g (Person name address age) = fmap (Person name address) (g age)

hollywood :: Text
hollywood = "Hollywood Blvd"

alice :: Person
alice = Person
  { _name = "Alice"
  , _address = Address
      { _street = hollywood
      , _city = "Los Angeles"
      }
  , _age = 30
  }

wilshire :: Text
wilshire = "Wilshire Blvd"

aliceWilshire :: Person
aliceWilshire = set (address.street) wilshire alice 

getStreet :: Person -> Text
getStreet = view (address.street)

-- | Increase age by 1
birthday :: Person -> Person
birthday = over age (+1)

getAge :: Person -> Int
getAge = view age

main3 :: IO ()
main3 = hspec $ do
  it "lives on Wilshire" $
    _street (_address aliceWilshire) `shouldBe` wilshire
  it "getStreet works" $
    getStreet alice `shouldBe` hollywood
  it "birthday" $
    getAge (birthday alice) `shouldBe` _age alice + 1
