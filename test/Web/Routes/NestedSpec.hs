{-# LANGUAGE
    OverloadedStrings
  #-}

module Web.Routes.NestedSpec (spec) where

import Web.Routes.NestedSpec.Basic
import Network.Wai.Trans

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
        it "should respond with 200 and 'foo!'" $
        get "/foo" `shouldRespondWith`
        "" { matchStatus = 200
           , matchBody = Just "foo!"
           }
      describe "GET /baz" $
        it "should respond with 200 and 'baz!'" $
        get "/baz" `shouldRespondWith`
        "" { matchStatus = 200
           , matchBody = Just "baz!"
           }
      describe "GET /borked" $
        it "should respond with 404" $
        get "/borked" `shouldRespondWith`
        404
  describe "Regex Routes" $
    with (return app) $ do
      describe "GET /athan@emails.com" $
        it "should respond with 200 and '[\"athan@emails.com\"] email'" $
        get "/athan@emails.com" `shouldRespondWith`
        "" { matchStatus = 200
           , matchBody = Just "[\"athan@emails.com\"] email"
           }
  describe "Upload Routes" $
    with (return app) $ do
      describe "POST /baz" $
        it "should respond with 200 and 'Woah! Upload content!'" $
        post "/baz" "anything" `shouldRespondWith`
        "" { matchStatus = 200
           , matchBody = Just "Woah! Upload content!"
           }
  describe "Secure Routes" $
    with (return app) $ do
      describe "GET /foo/bar" $
        it "should respond with 401 and 'Unauthorized!'" $
        get "/foo/bar" `shouldRespondWith`
        "" { matchStatus = 401
           , matchBody = Just "Unauthorized!"
           }


