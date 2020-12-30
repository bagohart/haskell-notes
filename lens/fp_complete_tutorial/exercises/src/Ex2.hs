{-# LANGUAGE OverloadedStrings #-}

module Ex2 where

import Lens.Micro.Platform
import Data.Text (Text)
import Test.Hspec

data Address = Address
  { _street :: !Text
  , _city :: !Text
  }

street :: Lens' Address Text
street = lens _street (\address newStreet -> address { _street = newStreet })

city :: Lens' Address Text
city = lens _city (\address newCity -> address { _city = newCity })

data Person = Person
  { _name :: !Text
  , _address :: !Address
  , _age :: !Int
  }

name :: Lens' Person Text
name = lens _name (\person newName -> person { _name = newName })

address :: Lens' Person Address
address = lens _address (\person newAddress -> person { _address = newAddress })

age :: Lens' Person Int
age = lens _age (\person newAge -> person { _age = newAge })

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

main2 :: IO ()
main2 = hspec $ do
  it "lives on Wilshire" $
    _street (_address aliceWilshire) `shouldBe` wilshire
  it "getStreet works" $
    getStreet alice `shouldBe` hollywood
  it "birthday" $
    getAge (birthday alice) `shouldBe` _age alice + 1
