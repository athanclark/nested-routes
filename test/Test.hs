module Main where

import Test.Hspec (hspec)
import Spec (spec)
import Test.Tasty
import Test.Tasty.Hspec


main :: IO ()
main = do
  r <- testSpec "basic" spec
  defaultMain r
