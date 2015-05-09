{-# LANGUAGE OverloadedStrings #-}


module Web.Routes.Nested.FileExtListener.Json where

import           Web.Routes.Nested.FileExtListener.Types
import           Web.Routes.Nested.FileExtListener.ByteString

import qualified Data.Aeson                              as A
import           Data.Map
import           Network.HTTP.Types                      (RequestHeaders,
                                                          Status, status200)
import           Network.Wai

import           Control.Monad.Writer



-- | Uses @Json@ as the key in the map, and @"application/json"@ as the content type.
json :: ( A.ToJSON j
        , Monad m ) =>
        j -> FileExtListenerT Response m ()
json = jsonStatusHeaders status200 [("Content-Type", "application/json")]

jsonStatus :: ( A.ToJSON j
        , Monad m ) =>
        Status -> j -> FileExtListenerT Response m ()
jsonStatus s = jsonStatusHeaders s [("Content-Type", "application/json")]

-- | Uses @Json@ as the key in the map, and @"application/javascript"@ as the content type.
jsonp :: ( A.ToJSON j
        , Monad m ) =>
        j -> FileExtListenerT Response m ()
jsonp = jsonStatusHeaders status200 [("Content-Type", "application/javascript")]

jsonpStatus :: ( A.ToJSON j
        , Monad m ) =>
        Status -> j -> FileExtListenerT Response m ()
jsonpStatus s = jsonStatusHeaders s [("Content-Type", "application/javascript")]

jsonHeaders :: ( A.ToJSON j
        , Monad m ) =>
        RequestHeaders -> j -> FileExtListenerT Response m ()
jsonHeaders = jsonStatusHeaders status200

jsonStatusHeaders :: ( A.ToJSON j
        , Monad m ) =>
        Status -> RequestHeaders -> j -> FileExtListenerT Response m ()
jsonStatusHeaders s hs i =
  let r = jsonOnlyStatusHeaders s hs i in
  FileExtListenerT $ tell $
    FileExts $ singleton Json r




jsonOnly :: A.ToJSON j =>
            j -> Response
jsonOnly = jsonOnlyStatusHeaders status200 [("Content-Type", "application/json")]

jsonOnlyStatus :: A.ToJSON j =>
            Status -> j -> Response
jsonOnlyStatus s = jsonOnlyStatusHeaders s [("Content-Type", "application/json")]

jsonpOnly :: A.ToJSON j =>
            j -> Response
jsonpOnly = jsonOnlyStatusHeaders status200 [("Content-Type", "application/javascript")]

jsonpOnlyStatus :: A.ToJSON j =>
            Status -> j -> Response
jsonpOnlyStatus s = jsonOnlyStatusHeaders s [("Content-Type", "application/javascript")]

jsonOnlyHeaders :: A.ToJSON j =>
            RequestHeaders -> j -> Response
jsonOnlyHeaders = jsonOnlyStatusHeaders status200

jsonOnlyStatusHeaders :: A.ToJSON j =>
            Status -> RequestHeaders -> j -> Response
jsonOnlyStatusHeaders s hs i =
  bytestringOnlyStatus s hs $ A.encode i

