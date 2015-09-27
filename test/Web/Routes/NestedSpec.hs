{-# LANGUAGE
    OverloadedStrings
  #-}

module Web.Routes.NestedSpec (spec) where

import Web.Routes.NestedSpec.Basic
-- import Web.Routes.Nested as N
import Network.Wai.Trans

import Test.Hspec
import Test.Hspec.Wai as HW

import Control.Monad


--withApp = withApplication . route

spec :: Spec
spec =
  describe "Literal Routes" $
    with (return app) $ do
      describe "GET /" $
        it "should respond with 200" $
        get "/" `shouldRespondWith`
        200




--    forM_ [ ("GET", N.get, HW.get)
--          , ("DELETE", N.delete, HW.delete)
--          ] $ \(method, verb, makeRequest) ->
--              describe method $
--                withApp (handle o (Just $ verb $ text "") Nothing) $
--                  it ("adds a route for " ++ method) $
--                    makeRequest "/" `shouldRespondWith` 200
