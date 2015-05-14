{-# LANGUAGE
    OverloadedStrings
  #-}

module Web.Routes.NestedSpec (main, spec) where

import Web.Routes.Nested
import Lucid hiding (with)
import Text.Lucius
import Network.Wai.Handler.Warp

import Test.Hspec
import Test.Hspec.Wai


main :: IO ()
main = hspec spec

withApp = with . run 3000 . route

spec :: Spec
spec = do
  describe "File Extensions" $ do
    it "should lookup literally" $ do
      "foo" == "foo"
