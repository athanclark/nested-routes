module Main where

import Spec (spec)
import Test.Tasty (defaultMain)
import Test.Tasty.Hspec (testSpec)


main :: IO ()
main = do
  r <- testSpec "basic" spec
  defaultMain r
