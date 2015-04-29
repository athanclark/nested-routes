{-# LANGUAGE OverloadedStrings #-}


module Web.Routes.Nested.FileExtListener.Json where

import           Web.Routes.Nested.FileExtListener.Types

import qualified Data.Aeson                              as A
import           Data.Map
import           Network.HTTP.Types                      (RequestHeaders,
                                                          Status, status200)
import           Network.Wai

import           Control.Monad.Writer




json :: ( A.ToJSON j
        , Monad m ) =>
        j -> FileExtListenerT Response m ()
json i =
  let r = responseLBS status200 [("Content-Type", "application/json")] $
            A.encode i in
  FileExtListenerT $ tell $
    FileExts $ singleton Json r

jsonStatus :: ( A.ToJSON j
        , Monad m ) =>
        Status -> j -> FileExtListenerT Response m ()
jsonStatus s i =
  let r = responseLBS s [("Content-Type", "application/json")] $
            A.encode i in
  FileExtListenerT $ tell $
    FileExts $ singleton Json r

jsonp :: ( A.ToJSON j
        , Monad m ) =>
        j -> FileExtListenerT Response m ()
jsonp i =
  let r = responseLBS status200 [("Content-Type", "application/javascript")] $
            A.encode i in
  FileExtListenerT $ tell $
    FileExts $ singleton Json r

jsonpStatus :: ( A.ToJSON j
        , Monad m ) =>
        Status -> j -> FileExtListenerT Response m ()
jsonpStatus s i =
  let r = responseLBS s [("Content-Type", "application/javascript")] $
            A.encode i in
  FileExtListenerT $ tell $
    FileExts $ singleton Json r

jsonHeaders :: ( A.ToJSON j
        , Monad m ) =>
        RequestHeaders -> j -> FileExtListenerT Response m ()
jsonHeaders hs i =
  let r = responseLBS status200 hs $
            A.encode i in
  FileExtListenerT $ tell $
    FileExts $ singleton Json r

jsonStatusHeaders :: ( A.ToJSON j
        , Monad m ) =>
        Status -> RequestHeaders -> j -> FileExtListenerT Response m ()
jsonStatusHeaders s hs i =
  let r = responseLBS s hs $
            A.encode i in
  FileExtListenerT $ tell $
    FileExts $ singleton Json r




jsonOnly :: A.ToJSON j =>
            j -> Response
jsonOnly i =
  responseLBS status200 [("Content-Type", "application/json")] $ A.encode i

jsonOnlyStatus :: A.ToJSON j =>
            Status -> j -> Response
jsonOnlyStatus s i =
  responseLBS s [("Content-Type", "application/json")] $ A.encode i

jsonpOnly :: A.ToJSON j =>
            j -> Response
jsonpOnly i =
  responseLBS status200 [("Content-Type", "application/javascript")] $ A.encode i

jsonpOnlyStatus :: A.ToJSON j =>
            Status -> j -> Response
jsonpOnlyStatus s i =
  responseLBS s [("Content-Type", "application/javascript")] $ A.encode i

jsonOnlyHeaders :: A.ToJSON j =>
            RequestHeaders -> j -> Response
jsonOnlyHeaders hs i =
  responseLBS status200 hs $ A.encode i

jsonOnlyStatusHeaders :: A.ToJSON j =>
            Status -> RequestHeaders -> j -> Response
jsonOnlyStatusHeaders s hs i =
  responseLBS s hs $ A.encode i

