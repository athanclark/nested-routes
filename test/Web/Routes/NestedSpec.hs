module Web.Routes.NestedSpec (main, spec) where

import Web.Routes.Nested

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "someFunction" $ do
    it "should work fine" $ do
      property someFunction

someFunction :: Bool -> Bool -> Property
someFunction x y = x === y
