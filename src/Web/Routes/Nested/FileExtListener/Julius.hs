{-# LANGUAGE OverloadedStrings #-}



module Web.Routes.Nested.FileExtListener.Julius where

import           Web.Routes.Nested.FileExtListener.Types
import           Web.Routes.Nested.FileExtListener.ByteString

import           Data.Map
import           Text.Julius
import qualified Data.Text.Lazy.Encoding                 as LT
import           Network.HTTP.Types                      (RequestHeaders,
                                                          Status, status200)
import           Network.Wai

import           Control.Monad.Writer


-- | Uses @julius@ as the key in the map, and @"application/javascript"@ as the content type.
julius :: Monad m => Javascript -> FileExtListenerT Response m ()
julius = juliusStatusHeaders status200 [("Content-Type", "application/javascript")]

juliusWith :: Monad m => (Response -> Response) -> Javascript -> FileExtListenerT Response m ()
juliusWith f = juliusStatusHeadersWith f status200 [("Content-Type", "application/javascript")]

juliusStatus :: Monad m => Status -> Javascript -> FileExtListenerT Response m ()
juliusStatus s = juliusStatusHeaders s [("Content-Type", "application/javascript")]

juliusStatusWith :: Monad m => (Response -> Response) -> Status -> Javascript -> FileExtListenerT Response m ()
juliusStatusWith f s = juliusStatusHeadersWith f s [("Content-Type", "application/javascript")]

juliusHeaders :: Monad m => RequestHeaders -> Javascript -> FileExtListenerT Response m ()
juliusHeaders = juliusStatusHeaders status200

juliusHeadersWith :: Monad m => (Response -> Response) -> RequestHeaders -> Javascript -> FileExtListenerT Response m ()
juliusHeadersWith f = juliusStatusHeadersWith f status200

juliusStatusHeaders :: Monad m => Status -> RequestHeaders -> Javascript -> FileExtListenerT Response m ()
juliusStatusHeaders = juliusStatusHeadersWith id

juliusStatusHeadersWith :: Monad m => (Response -> Response) -> Status -> RequestHeaders -> Javascript -> FileExtListenerT Response m ()
juliusStatusHeadersWith f s hs i =
  let r = juliusOnlyStatusHeaders s hs i in
  FileExtListenerT $ tell $
    FileExts $ singleton JavaScript $ f r




juliusOnly :: Javascript -> Response
juliusOnly = juliusOnlyStatusHeaders status200 [("Content-Type", "application/javascript")]

juliusOnlyStatus :: Status -> Javascript -> Response
juliusOnlyStatus s = juliusOnlyStatusHeaders s [("Content-Type", "application/javascript")]

juliusOnlyHeaders :: RequestHeaders -> Javascript -> Response
juliusOnlyHeaders = juliusOnlyStatusHeaders status200

juliusOnlyStatusHeaders :: Status -> RequestHeaders -> Javascript -> Response
juliusOnlyStatusHeaders s hs i = bytestringOnlyStatus s hs $ LT.encodeUtf8 $ renderJavascript i
