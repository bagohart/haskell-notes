#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
import Lens.Micro.Platform
import Data.Text (Text)
import Test.Hspec

data Address = Address
  { _street :: !Text
  , _city :: !Text
  }

makeLenses ''Address

data Person = Person
  { _name :: !Text
  , _address :: !Address
  , _age :: !Int
  }

makeLenses ''Person

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
aliceWilshire = _ -- FIXME set Alice's street to Wilshire

getStreet :: Person -> Text
getStreet = _

-- | Increase age by 1
birthday :: Person -> Person
birthday = _

getAge :: Person -> Int
getAge = _

main :: IO ()
main = hspec $ do
  it "lives on Wilshire" $
    _street (_address aliceWilshire) `shouldBe` wilshire
  it "getStreet works" $
    getStreet alice `shouldBe` hollywood
  it "birthday" $
    getAge (birthday alice) `shouldBe` _age alice + 1
