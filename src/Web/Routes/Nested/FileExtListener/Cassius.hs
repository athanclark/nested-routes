{-# LANGUAGE OverloadedStrings #-}



module Web.Routes.Nested.FileExtListener.Cassius where

import           Web.Routes.Nested.FileExtListener.Types as FE
import           Web.Routes.Nested.FileExtListener.ByteString

import           Data.Map
import           Text.Cassius
import qualified Data.Text.Lazy                          as LT
import qualified Data.Text.Lazy.Encoding                 as LT
import           Network.HTTP.Types                      (RequestHeaders,
                                                          Status, status200)
import           Network.Wai

import           Control.Monad.Writer


-- | Uses @cassius@ as the key in the map, and @"cassius/plain"@ as the content type.
cassius :: Monad m => Css -> FileExtListenerT Response m ()
cassius = cassiusStatusHeaders status200 [("Content-Type", "cassius/css")]

cassiusStatus :: Monad m => Status -> Css -> FileExtListenerT Response m ()
cassiusStatus s = cassiusStatusHeaders s [("Content-Type", "cassius/css")]

cassiusHeaders :: Monad m => RequestHeaders -> Css -> FileExtListenerT Response m ()
cassiusHeaders = cassiusStatusHeaders status200

cassiusStatusHeaders :: Monad m => Status -> RequestHeaders -> Css -> FileExtListenerT Response m ()
cassiusStatusHeaders s hs i =
  let r = cassiusOnlyStatusHeaders s hs i in
  FileExtListenerT $ tell $
    FileExts $ singleton Css r




cassiusOnly :: Css -> Response
cassiusOnly = cassiusOnlyStatusHeaders status200 [("Content-Type", "cassius/css")]

cassiusOnlyStatus :: Status -> Css -> Response
cassiusOnlyStatus s = cassiusOnlyStatusHeaders s [("Content-Type", "cassius/css")]

cassiusOnlyHeaders :: RequestHeaders -> Css -> Response
cassiusOnlyHeaders = cassiusOnlyStatusHeaders status200

cassiusOnlyStatusHeaders :: Status -> RequestHeaders -> Css -> Response
cassiusOnlyStatusHeaders s hs i = bytestringOnlyStatus s hs $ LT.encodeUtf8 $ renderCss i




