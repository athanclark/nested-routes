{-# LANGUAGE
    OverloadedStrings
  #-}

module Web.Routes.NestedSpec (spec) where

import Web.Routes.NestedSpec.Basic

import Test.Hspec
import Test.Hspec.Wai as HW



spec :: Spec
spec = do
  describe "Literal Routes" $
    with (return app) $ do
      describe "GET /" $
        it "should respond with 200" $
        get "/" `shouldRespondWith`
        200
      describe "GET /foo" $
        it "should respond with 200" $
        get "/foo" `shouldRespondWith`
        200
      describe "GET /baz" $
        it "should respond with 200" $
        get "/baz" `shouldRespondWith`
        200
      describe "GET /borked" $
        it "should respond with 404" $
        get "/borked" `shouldRespondWith`
        404
  describe "Attoparsec Routes" $
    with (return app) $ do
      describe "GET /12.34" $
        it "should respond with 200" $
        get "/12.34" `shouldRespondWith`
        200
  describe "Regex Routes" $
    with (return app) $ do
      describe "GET /athan@emails.com" $
        it "should respond with 200" $
        get "/athan@emails.com" `shouldRespondWith`
        200
  describe "Secure Routes" $
    with (return app) $ do
      describe "GET /foo/bar" $
        it "should respond with 401" $
        get "/foo/bar" `shouldRespondWith`
        401


