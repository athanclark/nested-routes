{-# LANGUAGE
    OverloadedStrings
  #-}

module Web.Routes.NestedSpec (main, spec) where

import Web.Routes.Nested as N
import Lucid hiding (with)
import Text.Lucius
import Network.Wai.Handler.Warp
import Network.Wai (Application)

import Test.Hspec
import Test.Hspec.Wai as HW

import Control.Monad


main :: IO ()
main = hspec spec

withApp = withApplication . route

spec :: Spec
spec =
  describe "HTTP Verbs" $
    forM_ [ ("GET", N.get, HW.get)
          , ("DELETE", N.delete, HW.delete)
          ] $ \(method, verb, makeRequest) ->
              describe method $
                withApp (handle o (Just $ verb $ text "") Nothing) $
                  it ("adds a route for " ++ method) $
                    makeRequest "/" `shouldRespondWith` 200
