{-# LANGUAGE OverloadedStrings #-}



module Web.Routes.Nested.FileExtListener.Julius where

import           Web.Routes.Nested.FileExtListener.Types
import           Web.Routes.Nested.FileExtListener.ByteString

import           Data.Map
import           Text.Julius
import qualified Data.Text.Lazy                          as LT
import qualified Data.Text.Lazy.Encoding                 as LT
import           Network.HTTP.Types                      (RequestHeaders,
                                                          Status, status200)
import           Network.Wai

import           Control.Monad.Writer


-- | Uses @julius@ as the key in the map, and @"application/javascript"@ as the content type.
julius :: Monad m => Javascript -> FileExtListenerT Response m ()
julius = juliusStatusHeaders status200 [("Content-Type", "application/javascript")]

juliusStatus :: Monad m => Status -> Javascript -> FileExtListenerT Response m ()
juliusStatus s = juliusStatusHeaders s [("Content-Type", "application/javascript")]

juliusHeaders :: Monad m => RequestHeaders -> Javascript -> FileExtListenerT Response m ()
juliusHeaders = juliusStatusHeaders status200

juliusStatusHeaders :: Monad m => Status -> RequestHeaders -> Javascript -> FileExtListenerT Response m ()
juliusStatusHeaders s hs i =
  let r = juliusOnlyStatusHeaders s hs i in
  FileExtListenerT $ tell $
    FileExts $ singleton JavaScript r




juliusOnly :: Javascript -> Response
juliusOnly = juliusOnlyStatusHeaders status200 [("Content-Type", "application/javascript")]

juliusOnlyStatus :: Status -> Javascript -> Response
juliusOnlyStatus s = juliusOnlyStatusHeaders s [("Content-Type", "application/javascript")]

juliusOnlyHeaders :: RequestHeaders -> Javascript -> Response
juliusOnlyHeaders = juliusOnlyStatusHeaders status200

juliusOnlyStatusHeaders :: Status -> RequestHeaders -> Javascript -> Response
juliusOnlyStatusHeaders s hs i = bytestringOnlyStatus s hs $ LT.encodeUtf8 $ renderJavascript i





