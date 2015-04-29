{-# LANGUAGE
    OverloadedStrings
  #-}


module Web.Routes.Nested.FileExtListener.Json where

import Web.Routes.Nested.FileExtListener.Types

import qualified Data.Aeson              as A
import Data.Map
import           Network.HTTP.Types      (status200, RequestHeaders, Status)
import           Network.Wai

import           Control.Monad.Writer


json :: (A.ToJSON j, Monad m) =>
        j -> FileExtListenerT Response m ()
json i =
  let r = responseLBS status200 [("Content-Type", "application/json")] $
            A.encode i in
  FileExtListenerT $ tell $
    FileExts $ singleton Json r

jsonOnly :: (A.ToJSON j) =>
            j -> Response
jsonOnly i =
  responseLBS status200 [("Content-Type", "application/json")] $ A.encode i

jsonp :: (A.ToJSON j, Monad m) =>
         j -> FileExtListenerT Response m ()
jsonp i =
  let r = responseLBS status200 [("Content-Type", "application/javascript")] $
            A.encode i in
  FileExtListenerT $ tell $
    FileExts $ singleton Json r

jsonpOnly :: (A.ToJSON j) =>
            j -> Response
jsonpOnly i =
  responseLBS status200 [("Content-Type", "application/javascript")] $ A.encode i
