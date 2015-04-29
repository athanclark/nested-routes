{-# LANGUAGE OverloadedStrings #-}


module Web.Routes.Nested.FileExtListener.Blaze where

import           Web.Routes.Nested.FileExtListener.Types

import           Data.Map
import qualified Data.Text.Lazy.Encoding                 as LT
import           Network.HTTP.Types                      (RequestHeaders,
                                                          Status, status200)
import           Network.Wai
import qualified Text.Blaze.Html                         as H
import qualified Text.Blaze.Html.Renderer.Text           as H

import           Control.Monad.Writer



blaze :: Monad m => H.Html -> FileExtListenerT Response m ()
blaze i =
  let r = responseLBS status200 [("Content-Type", "text/html")] $
            LT.encodeUtf8 $ H.renderHtml i in
  FileExtListenerT $ tell $
    FileExts $ singleton Html r

blazeHeaders :: Monad m => RequestHeaders -> H.Html -> FileExtListenerT Response m ()
blazeHeaders hs i =
  let r = responseLBS status200 hs $
            LT.encodeUtf8 $ H.renderHtml i in
  FileExtListenerT $ tell $
    FileExts $ singleton Html r

blazeStatus :: Monad m => Status -> H.Html -> FileExtListenerT Response m ()
blazeStatus s i =
  let r = responseLBS s [("Content-Type", "text/html")] $
            LT.encodeUtf8 $ H.renderHtml i in
  FileExtListenerT $ tell $
    FileExts $ singleton Html r

blazeStatusHeaders :: Monad m => Status -> RequestHeaders -> H.Html -> FileExtListenerT Response m ()
blazeStatusHeaders s hs i =
  let r = responseLBS s hs $
            LT.encodeUtf8 $ H.renderHtml i in
  FileExtListenerT $ tell $
    FileExts $ singleton Html r



blazeOnly :: H.Html -> Response
blazeOnly i =
  responseLBS status200 [("Content-Type", "text/html")] $ LT.encodeUtf8 $ H.renderHtml i

blazeOnlyHeaders :: RequestHeaders -> H.Html -> Response
blazeOnlyHeaders hs i =
  responseLBS status200 hs $ LT.encodeUtf8 $ H.renderHtml i

blazeOnlyStatus :: Status -> H.Html -> Response
blazeOnlyStatus s i =
  responseLBS s [("Content-Type", "text/html")] $ LT.encodeUtf8 $ H.renderHtml i

blazeOnlyStatusHeaders :: Status -> RequestHeaders -> H.Html -> Response
blazeOnlyStatusHeaders s hs i =
  responseLBS s hs $ LT.encodeUtf8 $ H.renderHtml i


